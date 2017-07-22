Imports System.Windows.Forms

Public Class SelectionDataObject
    Implements IDataObject

    Friend SL As SelectionManager
    Friend hb As HexBox
    Friend list As List(Of String)
    Friend myFormat As String
    Friend byteStart As Long
    Friend length As Long

    Public Sub New(ByVal SL As SelectionManager)
        Me.SL = SL
        Me.hb = SL.tb

        list = New List(Of String)
        list.Add(DataFormats.StringFormat)
        list.Add(DataFormats.Text)
        list.Add(DataFormats.UnicodeText)
        list.Add(DataFormats.FileDrop)
        myFormat = GetType(SelectionDataObject).ToString
        list.Add(myFormat)
    End Sub
    Public Sub New(ByVal HB As HexBox, ByVal byteStart As Long, ByVal length As Long)
        Me.hb = HB
        Me.byteStart = byteStart
        Me.length = length
    End Sub

    Public Sub SetData(data As Object) Implements IDataObject.SetData
        Throw New NotImplementedException()
    End Sub

    Public Sub SetData(format As Type, data As Object) Implements IDataObject.SetData
        Throw New NotImplementedException()
    End Sub

    Public Sub SetData(format As String, data As Object) Implements IDataObject.SetData
        Throw New NotImplementedException()
    End Sub

    Public Sub SetData(format As String, autoConvert As Boolean, data As Object) Implements IDataObject.SetData
        Throw New NotImplementedException()
    End Sub

    Public Function GetData(format As Type) As Object Implements IDataObject.GetData
        Return Me.GetData(format.ToString, True)
    End Function

    Public Function GetData(format As String) As Object Implements IDataObject.GetData
        Return Me.GetData(format, True)
    End Function

    Public Function GetData(format As String, autoConvert As Boolean) As Object Implements IDataObject.GetData
        Try
            Select Case format
                Case DataFormats.StringFormat, DataFormats.Text, DataFormats.UnicodeText
                    Return "abc"
                Case myFormat
                    Return Me
                Case DataFormats.FileDrop
                    Return {"C:\Users\Faishal\Documents\Visual Studio 2015\Projects\HexProject\HexProject\bin\Debug\test.txt"}
                Case Else
                    Return Nothing
            End Select
        Catch ex As Exception
            Return Nothing
        End Try
    End Function

    Public Function GetDataPresent(format As String, autoConvert As Boolean) As Boolean Implements IDataObject.GetDataPresent
        Select Case format
            Case DataFormats.StringFormat
                Return True
            Case DataFormats.Text, DataFormats.UnicodeText
                Return True
            Case DataFormats.FileDrop
                Return True
            Case myFormat
                Return True
            Case Else
                Return False
        End Select
    End Function

    Public Function GetDataPresent(format As String) As Boolean Implements IDataObject.GetDataPresent
        Return GetDataPresent(format, True)
    End Function

    Public Function GetDataPresent(format As Type) As Boolean Implements IDataObject.GetDataPresent
        Return GetDataPresent(format.ToString, True)
    End Function

    Public Function GetFormats(autoConvert As Boolean) As String() Implements IDataObject.GetFormats
        Return List.ToArray
    End Function

    Public Function GetFormats() As String() Implements IDataObject.GetFormats
        Return GetFormats(True)
    End Function
End Class