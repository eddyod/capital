library(shiny)


paramNames <- c("start_capital", "annual_mean_return", "annual_ret_std_dev",
"annual_inflation", "annual_inf_std_dev", "monthly_deposits", "monthly_withdrawals", "years_to_save",
"n_sim", "ssn", "ssn_age", "monthly_pension", "house_sale", "when_house_sale", "income_other")

shinyServer(function(input, output, session) {

 getParams <- function(prefix) {
 input[[paste0(prefix, "_recalc")]]

 params <- lapply(paramNames, function(p) {
 input[[paste0(prefix, "_", p)]]
 })
 names(params) <- paramNames
 params
 }

  # Function that generates scenarios and computes NAV. The expression
  # is wrapped in a call to reactive to indicate that:
  #
  # 1) It is "reactive" and therefore should be automatically
  # re-executed when inputs change
  #
  navA <- reactive(do.call(
  simulate_nav, 
  getParams("a"))
  )
  
  output$a_distPlot <- renderPlot({
   plot_nav(navA(), output, getParams("a"))
  })
  

})


simulate_nav <- function(start_capital = 215000, annual_mean_return = 4, annual_ret_std_dev = 5, 
                         annual_inflation = 2, annual_inf_std_dev = 2, monthly_deposits = 2700, monthly_withdrawals = 4200,
                         years_to_save = 3, n_sim = 10, ssn = 1200, ssn_age = 10, monthly_pension = 2133,
                         house_sale = 250000, when_house_sale = 10,
                         start_other = 3, end_other = 10 , income_other = 500) {
# get percentage - Investment
annual.mean.return = annual_mean_return / 100
annual.ret.std.dev = annual_ret_std_dev / 100
        
# get percentage -  Inflation
annual.inflation = annual_inflation / 100
annual.inf.std.dev = annual_inf_std_dev / 100
                
# number of months to simulate
months_to_save = 12 * years_to_save
# number of years to live
n_live = 80 * 12
n_obs_n_live = months_to_save + n_live
start_ssn = ssn_age * 12
when_house_sale = when_house_sale * 12
# other income
start_other = start_other * 12
end_other = end_other * 12                                        

# monthly Investment and Inflation assumptions
monthly.mean.return = annual.mean.return / 12
monthly.ret.std.dev = annual.ret.std.dev / sqrt(12)

monthly.inflation = annual.inflation / 12
monthly.inf.std.dev = annual.inf.std.dev / sqrt(12)
# simulate Returns
monthly.invest.returns = matrix(0, n_obs_n_live, n_sim)
monthly.inflation.returns = matrix(0, n_obs_n_live, n_sim)

monthly.invest.returns[] = rnorm(n_obs_n_live  * n_sim, mean = monthly.mean.return, sd = monthly.ret.std.dev)
monthly.inflation.returns[] = rnorm(n_obs_n_live * n_sim, mean = monthly.inflation, sd = monthly.inf.std.dev)

                                                                  
# setup/initialize matrices
finish <- n_obs_n_live + 1
me_dead <- 12 * 30
nav <- matrix(0, finish, n_sim)
withdrawal_matrix <- matrix(0, finish, n_sim)
ssn_matrix <- matrix(0, finish, n_sim)
pension_matrix <- matrix(0, finish, n_sim)
other_matrix <- matrix(0, finish, n_sim)
#fill matrices
nav[1, ] <- start_capital
nav[2:months_to_save, ] <- monthly_deposits
nav[when_house_sale, ] <- house_sale
finish_saving <- months_to_save + 1

withdrawal_matrix[finish_saving:me_dead, ] <- monthly_withdrawals
withdrawal_matrix[me_dead:finish, ] <- monthly_withdrawals * 0.5

ssn_matrix[start_ssn:finish, ] <- ssn

pension_matrix[finish_saving:me_dead, ] <- monthly_pension
pension_matrix[me_dead:finish, ] <- monthly_pension * 0.5

other_matrix[start_other:end_other, ] <- income_other
# add em up
nav <- nav - withdrawal_matrix + other_matrix + ssn_matrix + pension_matrix
# compute random flucuations
for (j in 1:n_obs_n_live) {
 nav[j + 1, ] = nav[j + 1, ] + nav[j, ] * (1 + monthly.invest.returns[j, ] - monthly.inflation.returns[j, ])
 mean_sim = mean(nav[j,])
 if (mean_sim <= 0) {
  break
 }

}

nav = nav[1:j, ]
nav = nav / 1000

return(nav)
}


plot_nav <- function(nav, output, params) {
  now <- as.numeric(format(Sys.time(),"%Y"))
  n <- nrow(nav) 
  yearLabels <- seq(now+1,now + round(n/12),1)
  l <- length(yearLabels)
  top <- round(n/l)
  myat <- (yearLabels - now) * top
  last_year = tail(yearLabels, n = 1)
  diff_year = last_year - now
  mnav <- rowMeans(nav[, -ncol(nav)])
  money_left <- tail(mnav, n=1)
  dollar_money_left <- paste("$",format(money_left*1000, big.mark=","),sep="")
  x <- diff_year * 12 - 50
  me_dead <- 30 * 12
  # plot all scenarios
  matplot(nav,
    type = 'l', lwd = 0.5, lty = 1, col = rainbow(ncol(nav)),
    xaxt = 'n', ylab = 'Dollar Amount in Thousands', sub='Projected Value Over Time', cex.sub=2)
    axis(3,at=myat,labels=yearLabels,cex.axis=1.25,tck=0.01)
    lines(mnav, col = 'black', lwd = 3)
    text(params$ssn_age*12, money_left, "SSN Starts", offset = 5, srt=45)
    abline(v = params$ssn_age*12, col="green", lwd=1, lty=2)
    text(x, money_left, paste("Money left:",dollar_money_left), pos=3, srt=45)
    # text(me_dead, money_left, "I'm Dead", srt=45)
    text(params$years_to_save*12,money_left, "Retire", srt=45)
    abline(v = params$years_to_save*12, col="red", lwd=1, lty=2)
    # abline(v = me_dead, col="purple", lwd=1, lty=2)
    abline(h = money_left, col="blue", lwd=1, lty=2)
    
    points(diff_year*12,money_left, pch=19)
    grid()
    if (diff_year > 70) {
     out <- paste("Your money has not run out. Simulation ended in year:",last_year,"Money left is",dollar_money_left)
    } else {
     out <- paste("Your money will run out in the year:",last_year,"(",diff_year,"years from now.)")
    }
  output$summary <- renderPrint({
    out
  })

}
