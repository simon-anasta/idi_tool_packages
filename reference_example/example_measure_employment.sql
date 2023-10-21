/******************************************************************************
Title: Employment infered from monthly tax returns

Description:
A spell where wages or salaries are reported to IRD as evidence of employment.

Intended purpose:
Creating indicators of when/whether a person was employed.
Summing total income from Wages and Salaries.

Inputs & Dependencies:
- [IDI_Clean].[ir_clean].[ird_ems]
Outputs:
- [IDI_UserCode].[DL-MAA20XX-YY].[defn_employed]

 Notes:
1) Self employment does not appear in this definition.
2) Not suited to counting days employed as people working multiple jobs will
   count the same day twice.
3) A placeholder identity exists where the encrypted IRD number [snz_ird_uid]
   is unavailable. We exclude this identity.

******************************************************************************/

USE IDI_UserCode
GO

DROP VIEW IF EXISTS [DL-MAA20XX-YY].[defn_employed];
GO

CREATE VIEW [DL-MAA20XX-YY].[defn_employed] AS
SELECT snz_uid
	,DATEFROMPARTS(YEAR([ir_ems_return_period_date]), MONTH([ir_ems_return_period_date]), 1) AS period_start_date
	,[ir_ems_return_period_date]
	,ir_ems_gross_earnings_amt
FROM [IDI_Clean].[ir_clean].[ird_ems]
WHERE [ir_ems_income_source_code]= 'W&S' -- income from wages and salaries
AND [snz_ird_uid] <> 0; -- exclude placeholder person without IRD number
GO
