Option Explicit

' Check arguments
If WScript.Arguments.Count < 2 Then
    WScript.StdErr.WriteLine "Invalid arguments"
    WScript.Quit 1
End If

' Get arguments
Dim dbPath: dbPath = WScript.Arguments(0)
Dim eventId: eventId = WScript.Arguments(1)
Dim fso: Set fso = CreateObject("Scripting.FileSystemObject")

' Function to safely execute SQL with database existence check
Function SafeExecuteSQL(sql)
    ' First check if database still exists
    If Not fso.FileExists(dbPath) Then
        SafeExecuteSQL = ""
        Exit Function
    End If

    Dim tempConn: Set tempConn = CreateObject("ADODB.Connection")
    On Error Resume Next

    ' Try primary driver
    tempConn.Open "Driver={SQLite3 ODBC Driver};Database=" & dbPath & ";"
    If Err.Number <> 0 Then
        ' Try alternate driver
        Err.Clear
        tempConn.Open "Driver={SQLite ODBC Driver};Database=" & dbPath & ";"
    End If

    If Err.Number <> 0 Then
        ' Database connection failed - might have been removed
        SafeExecuteSQL = ""
        Exit Function
    End If
    On Error Goto 0

    ' Set timeout
    tempConn.Execute "PRAGMA busy_timeout=5000;"

    ' Execute query
    Dim result
    Dim rs: Set rs = tempConn.Execute(sql)
    If Not rs.EOF Then
        result = rs.Fields(0).Value
    Else
        result = ""
    End If

    ' Clean up
    rs.Close
    Set rs = Nothing
    tempConn.Close
    Set tempConn = Nothing

    SafeExecuteSQL = result
End Function

' Execute the R script
Dim shell: Set shell = CreateObject("WScript.Shell")
Dim rCmd: rCmd = "Rscript -e ""library(vigil); vigil:::execute_callback('" & _
                Replace(dbPath, "'", "''") & "', " & eventId & ")"""

' Launch R process
Dim proc: Set proc = shell.Exec(rCmd)
Dim rPid: rPid = proc.ProcessID

' Try to record the R process PID in the database
' If database is gone, continue anyway as R process is already running
SafeExecuteSQL "INSERT INTO active_processes (pid, type, event_id) " & _
              "VALUES (" & rPid & ", 'callback', " & eventId & ");"

' Wait for R script to complete
proc.StdOut.ReadAll

' Try to mark process as inactive
' If database is gone, just exit gracefully
SafeExecuteSQL "UPDATE active_processes SET active = FALSE " & _
              "WHERE pid = " & rPid & " AND type = 'callback';"

WScript.Quit 0
