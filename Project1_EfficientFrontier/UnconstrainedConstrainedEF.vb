'
' UnconstrainedEF Macro
' Written for Intro to Financial Engineering class (Project 1) at DePauw University
'
'@author: Shuto Araki
'@email: shutoaraki_2020@depauw.edu
'@date: 02/27/2017
' **Needs Solver reference by going to Tools -> References and choose Solver**
' Make sure you name variables as well:
'   minRet, maxRet, numOfPoints, rets, weights1, vcv, expReturn, expRisk, target1, and change
'
' Special thanks to Hassan Sysed Mehdi for pointing out the error in the formula for expected return.
'
Sub UnconstrainedEF()
    Dim clean As Range
    Dim minReturn, maxReturn, numOfPoints, deltaX
    'Variables
    minReturn = Range("minRet").Value
    maxReturn = Range("maxRet").Value
    numOfPoints = Range("numOfPoints").Value
    deltaX = (maxReturn - minReturn) / (numOfPoints - 1)

    'Clear the contents below
    Set clean = Range("M" & 38 + numOfPoints & ":U1000")
    clean.ClearContents

    'Use Solver to get values
    Dim i As Integer
    For i = 0 To numOfPoints - 1

        'Set Formulas and values
        Range("expReturn").Formula = "=MMULT(weights1, rets)"
        Range("expRisk").FormulaArray = "=SQRT(MMULT(MMULT(weights1, vcv), TRANSPOSE(weights1)))"
        Range("target1").Value = minReturn + i * deltaX

        'Get Solver Results
        SolverReset
        SolverOptions Assumenonneg:=False
        Call SolverAdd(Range("expReturn"), 2, Range("target1"))
        Call SolverOk(Range("expRisk"), 2, 0, Range("change"))
        Call SolverSolve(True)
        SolverFinish

        'Copy and Paste values
        Range("Q23:Y23").Copy
        Range("M" & 38 + i & ":U" & 38 + i).PasteSpecial Paste:=xlValues

    Next i

    'Clear the clipboard
    Application.CutCopyMode = False

End Sub

'
' ConstrainedEF Macro
' Written for Intro to Financial Engineering class (Project 1) at DePauw University
'
'@author: Shuto Araki
'@email: shutoaraki_2020@depauw.edu
'@date: 02/27/2017
' **Needs Solver reference by going to Tools -> References and choose Solver**
' Make sure you name variables as well:
'   minRet, maxRet, numOfPoints, rets, weights1, vcv, expReturn, expRisk, target1, and change
'
Sub ConstrainedEF()
    Dim clean As Range
    Dim minReturn, maxReturn, numOfPoints, deltaX

    'Calculating the min portfolio expected return with constraits
    SolverReset
    Call SolverAdd(Range("weights1"), 1, 0.5) 'Adding weight <= 50% constraint
    Call SolverAdd(Range("weights1"), 3, 0.05) 'Adding weight >= 5% constraint
    Call SolverOk(Range("expReturn"), 2, 0, Range("change"))
    Call SolverSolve(True)
    SolverFinish
    minReturn = Range("expReturn").Value

    'Calculating the max portfolio expected return with constraits
    SolverReset
    Call SolverAdd(Range("weights1"), 1, 0.5) 'Adding weight <= 50% constraint
    Call SolverAdd(Range("weights1"), 3, 0.05) 'Adding weight >= 5% constraint
    Call SolverOk(Range("expReturn"), 1, 0, Range("change"))
    Call SolverSolve(True)
    SolverFinish
    maxReturn = Range("expReturn").Value


    numOfPoints = Range("numOfPoints").Value
    deltaX = (maxReturn - minReturn) / (numOfPoints - 1)

    'Clear the contents below
    Set clean = Range("M" & 38 + numOfPoints & ":U1000")
    clean.ClearContents

    'Use Solver to get values
    Dim i As Integer
    For i = 0 To numOfPoints - 1

        'Set Formulas and values
        Range("expReturn").Formula = "=MMULT(weights1, rets)"
        Range("expRisk").FormulaArray = "=SQRT(MMULT(MMULT(weights1, vcv), TRANSPOSE(weights1)))"
        Range("target1").Value = minReturn + i * deltaX

        'Get Solver Results
        SolverReset
        SolverOptions Assumenonneg:=False
        Call SolverAdd(Range("expReturn"), 2, Range("target1"))
        Call SolverAdd(Range("weights1"), 1, 0.5) 'Adding weight <= 50% constraint
        Call SolverAdd(Range("weights1"), 3, 0.05) 'Adding weight >= 5% constraint
        Call SolverOk(Range("expRisk"), 2, 0, Range("change"))
        Call SolverSolve(True)
        SolverFinish

        'Copy and Paste values
        Range("Q23:Y23").Copy
        Range("M" & 38 + i & ":U" & 38 + i).PasteSpecial Paste:=xlValues

    Next i

    'Clear the clipboard
    Application.CutCopyMode = False

End Sub
