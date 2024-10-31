#' Calculate NHS Take-Home Pay
#'
#' This function provides the value for take home pay, income tax, national insurance, pension contributions and  student loan repayments for staff on NHS AFC pay conditions.
#' @param annual_wage The annual wage before tax and other deductions.
#' @param income_tax_allowance The income tax allowance to be used. Defaults to the UK standard of 12570.
#' @param student_loan_plan Options are 1,2,4,5,'postgraduate' or 'none'. Defaults to 'none'.
#' @param period Specify whether the output should be the annual breakown, or a monthly breakdown. Accepts 'monthly' or 'annual'. Defaults to 'monthly'. Monthly breakdown is equivalent to 1/12th of annual values.
#' @export
#' @import dplyr
#' @import tidyr
#' @examples
#' nhs_payslip(annual_wage = 50000)


nhs_payslip <- function(annual_wage,
                          income_tax_allowance=12570,
                          student_loan_plan= 'none',
                          period='monthly'){
ni_allowance <- 12570
lower_tax_threshold <- 50270
higher_tax_threshold <- 125140
income_tax_lower_rate <- 0.2
income_tax_higher_rate <- 0.4
income_tax_max_rate <- 0.45
lower_ni_threshold <- 50270
lower_ni_rate <- 0.08
higher_ni_rate <- 0.02

pension_contribution_rate <- case_when(annual_wage < 13260 ~ 0.052,
                                       annual_wage < 26832 ~ 0.065,
                                       annual_wage < 32692 ~ 0.083,
                                       annual_wage < 49079 ~ 0.098,
                                       annual_wage < 62925 ~ 0.107,
                                       .default = 0.125)

student_loan_rate <- case_when(student_loan_plan == 1 ~ 0.09,
                               student_loan_plan == 2 ~ 0.09,
                               student_loan_plan == 4 ~ 0.09,
                               student_loan_plan == 5 ~ 0.09,
                               student_loan_plan == 'postgraduate' ~ 0.06,
                               .default = 0)
student_loan_threshold <- case_when(student_loan_plan == 1 ~ 24990,
                                    student_loan_plan == 2 ~ 27295,
                                    student_loan_plan == 4 ~ 31395,
                                    student_loan_plan == 5 ~ 25000,
                                    student_loan_plan == 'postgraduate' ~ 21000,
                                    .default = 0)

annual_pension <- annual_wage*pension_contribution_rate

annual_ni <- if_else(annual_wage>lower_ni_threshold,
                     ((lower_ni_threshold - ni_allowance)*lower_ni_rate)+
                        ((annual_wage - lower_ni_threshold)*higher_ni_rate),
                      ((annual_wage - ni_allowance)*lower_ni_rate))
annual_tax <- case_when((annual_wage - annual_pension)>higher_tax_threshold ~
                          (((annual_wage - annual_pension - higher_tax_threshold)*income_tax_max_rate+
                              ((higher_tax_threshold - lower_tax_threshold)*income_tax_higher_rate)+
                              ((lower_tax_threshold - income_tax_allowance)*income_tax_lower_rate))),
                        (annual_wage - annual_pension)>lower_tax_threshold ~
                          (((annual_wage - annual_pension - lower_tax_threshold)*income_tax_higher_rate)+
                             ((lower_tax_threshold - income_tax_allowance)*income_tax_lower_rate)),
                        .default = ((annual_wage - annual_pension - income_tax_allowance)*income_tax_lower_rate))

annual_student_loan <- if_else((annual_wage>student_loan_threshold),
                               (annual_wage - student_loan_threshold)*student_loan_rate,
                               0)
annual_take_home <- annual_wage -
  annual_pension -
  annual_ni -
  annual_tax -
  annual_student_loan

monthly_wage <- annual_wage/12
monthly_pension <- annual_pension/12
monthly_ni <- annual_ni/12
monthly_tax <- annual_tax/12
monthly_student_loan <- annual_student_loan/12
monthly_take_home <- annual_take_home/12


monthly_payslip <- tibble(monthly_wage,
       monthly_pension,
       monthly_ni,
       monthly_tax,
       monthly_student_loan,
       monthly_take_home) %>%
  pivot_longer(names_to = "Pay Element",
               values_to = "Amount",
               cols=everything())

annual_payslip <- tibble(annual_wage,
                         annual_pension,
                         annual_ni,
                         annual_tax,
                         annual_student_loan,
                         annual_take_home) %>%
  pivot_longer(names_to = "Pay Element",
               values_to = "Amount",
               cols=everything())

case_when(period == 'monthly' ~ (monthly_payslip),
          period == 'annual' ~ (annual_payslip))

}




