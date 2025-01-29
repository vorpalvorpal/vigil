' Usage: cscript callback-runner.vbs <database_path> <event_id>

Option Explicit

If WScript.Arguments.Count < 2 Then
    WScript.StdErr.WriteLine "Invalid arguments"
    WScript.Quit 1
End If

Dim dbPath: dbPath = WScript.Arguments(0)
Dim eventId: eventId = WScript.Arguments(1)

' Verify database exists
Dim fso: Set fso = CreateObject("Scripting.FileSystemObject")
If Not fso.FileExists(dbPath) Then
    WScript.StdErr.WriteLine "Database not found"
    WScript.Quit 1
End If

' Create temporary file for output
Dim tempPath: tempPath = fso.GetSpecialFolder(2) & "\" & fso.GetTempName()

' Setup database access using R wrapper
Function ExecuteSQL(sql)
    Dim shell: Set shell = CreateObject("WScript.Shell")
    Dim cmd: cmd = "Rscript sqlite-wrapper.R """ & dbPath & """ """ & _
                  Replace(sql, """", "\""") & """"
    Dim exec: Set exec = shell.Exec(cmd)
    ExecuteSQL = exec.StdOut.ReadAll
End Function

' Get callback content
Dim callbackContent: callbackContent = ExecuteSQL("SELECT value FROM config WHERE key='callback_content';")

If Len(callbackContent) = 0 Then
    WScript.StdErr.WriteLine "No callback configured"
    WScript.Quit 1
End If

' Run the callback in R and capture output
Dim shell: Set shell = CreateObject("WScript.Shell")
Dim rCmd: rCmd = "Rscript -e ""tryCatch({ " & _
                 "event <- DBI::dbGetQuery(" & _
                 "DBI::dbConnect(RSQLite::SQLite(), '" & Replace(dbPath, "'", "''") & "'), " & _
                 "'SELECT * FROM events WHERE id = " & eventId & "'); " & _
                 Replace(callbackContent, """", "\""") & _
                 "}, error = function(e) { " & _
                 "cat('Error in callback: ', conditionMessage(e), '\n') " & _
                 "})"" > """ & tempPath & """ 2>&1"

shell.Run rCmd, 0, True

' Read output and escape for SQL
Dim output: output = ""
If fso.FileExists(tempPath) Then
    Dim textStream: Set textStream = fso.OpenTextFile(tempPath, 1)
    output = Replace(textStream.ReadAll(), "'", "''")
    textStream.Close
    fso.DeleteFile tempPath
End If

' Update event with callback output and timestamp
Dim timestamp: timestamp = Year(Now) & "-" & Right("0" & Month(Now), 2) & "-" & _
               Right("0" & Day(Now), 2) & " " & Right("0" & Hour(Now), 2) & ":" & _
               Right("0" & Minute(Now), 2) & ":" & Right("0" & Second(Now), 2)

ExecuteSQL "UPDATE events " & _
          "SET callback_output = '" & output & "', " & _
          "    callback_timestamp = '" & timestamp & "' " & _
          "WHERE id = " & eventId
