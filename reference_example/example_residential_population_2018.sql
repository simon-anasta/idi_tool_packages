/******************************************************************************
Title: 2018 Residential population

Description: 
List of identities that resident in 2018

Intended purpose:
Producing summary statistics for the entire population.

Inputs & Dependencies:
- [IDI_Clean].[data].[snz_res_pop]
Outputs:
- [IDI_UserCode].[DL-MAA2020-01].[defn_2018_residents]

******************************************************************************/

USE IDI_UserCode
GO

DROP VIEW IF EXISTS [DL-MAA20XX-YY].[defn_2018_residents];
GO

CREATE VIEW [DL-MAA20XX-YY].[defn_2018_residents] AS
SELECT [snz_uid]
      ,[srp_ref_date]
FROM [IDI_Clean].[data].[snz_res_pop]
WHERE YEAR(srp_ref_date) = 2018;
GO
