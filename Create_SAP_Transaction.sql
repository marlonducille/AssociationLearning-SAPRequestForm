/****** Script for SelectTopNRows command from SSMS  ******/



DECLARE @TEMP_TABLE TABLE(SAPRequestID INT, SAPRoleName varchar(50))

INSERT INTO @TEMP_TABLE    
SELECT SAPRequestID, SAPRoleName FROM [SAPRequest].[dbo].[SAPRequestRoles] a
INNER JOIN  [SAPRequest].[dbo].SAPRoles b on b.SAPRoleID = a.SAPRoleID

SELECT  SAPRequestID, STUFF((SELECT ', ' + SAPRoleName 
         FROM @TEMP_TABLE 
         WHERE SAPRequestID = t.SAPRequestID
         FOR XML PATH(''), TYPE)
        .value('.','NVARCHAR(MAX)'),1,2,' ') List_Output
FROM @TEMP_TABLE t
GROUP BY SAPRequestID