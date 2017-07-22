Public Class AttributeByte
    Friend fblist As New List(Of RangeByte)
    Public Overridable Sub AddRange(ByVal position As Long, ByVal length As Long)
        Dim a1 As Long = position
        Dim a2 As Long = position + length
        Dim b1 As Long = 0
        Dim b2 As Long = 0
        Dim fnd As Boolean = False
        For Each i In fblist
            b1 = i.pos
            b2 = i.pos + i.len
            If (b1 <= a2) And (a1 <= b2) Then
                i.pos = Math.Min(a1, b1)
                i.len = Math.Max(a2, b2) - i.pos
                fnd = True
                Exit For
            End If
        Next
        If Not fnd Then
            fblist.Add(New RangeByte(position, length))
        Else
            'rearrange
            Do
                Dim ub1 As RangeByte = Nothing
                Dim ub2 As RangeByte = Nothing
                Dim ln As Integer = fblist.Count - 1
                Dim removeUB As New List(Of RangeByte)
                Dim c As Integer = 0
                For i As Integer = 0 To ln
                    ub1 = fblist(i)
                    a1 = ub1.pos
                    a2 = ub1.pos + ub1.len

                    For i2 As Integer = i + 1 To ln
                        ub2 = fblist(i2)
                        b1 = ub2.pos
                        b2 = ub2.pos + ub1.len

                        If (b1 <= a2) And (a1 <= b2) Then
                            ub1.pos = Math.Min(a1, b1)
                            ub1.len = Math.Max(a2, b2) - ub1.pos
                            removeUB.Add(ub2)
                            c += 1
                        End If
                    Next
                Next
                For Each i In removeUB
                    fblist.Remove(i)
                Next
                If c = 0 Then Exit Do
            Loop
        End If
    End Sub
    Public Overridable Sub RemoveRange(ByVal position As Long, ByVal length As Long)
        If length = 0 Then Exit Sub

        Dim a1 As Long = position
        Dim a2 As Long = position + length
        Dim b1 As Long = 0
        Dim b2 As Long = 0
        Dim removeUB As New List(Of RangeByte)
        Dim splitedUB As New List(Of RangeByte)

        For Each i In fblist
            b1 = i.pos
            b2 = i.pos + i.len
            If Helper.IntersectsWith(a1, a2, b1, b2) Then
                If a1 <= b1 Then
                    i.pos = a2
                    i.len = b2 - i.pos
                ElseIf a1 > b1 Then
                    If a2 >= b2 Then
                        i.len = a1 - b1
                    Else
                        splitedUB.Add(New RangeByte(a2, b2 - a2))
                        i.len = a1 - b1
                    End If
                End If
                If i.len < 1 Then
                    removeUB.Add(i)
                End If
            End If
        Next
        For Each i In removeUB
            fblist.Remove(i)
        Next
        For Each i In splitedUB
            fblist.Add(i)
        Next
    End Sub
    Public Overridable Function IsInRange(ByVal position As Long, ByVal length As Long) As Boolean
        If length = 0 Then Return False

        Dim a1 As Long = position
        Dim a2 As Long = position + length
        Dim b1 As Long = 0
        Dim b2 As Long = 0
        For Each i In fblist
            b1 = i.pos
            b2 = i.pos + i.len
            If Helper.IntersectsWith(a1, a2, b1, b2) Then Return True
        Next
        Return False
    End Function
    Public Overridable Sub Clear()
        fblist.Clear()
    End Sub
    Public ReadOnly Property Items As RangeByte()
        Get
            Return fblist.ToArray()
        End Get
    End Property
End Class
Public Class RangeByte
    Public pos As Long
    Public len As Long

    Public ReadOnly Property Position As Long
        Get
            Return pos
        End Get
    End Property
    Public ReadOnly Property Length As Long
        Get
            Return len
        End Get
    End Property

    Public Sub New(Position As Long, Length As Long)
        Me.pos = Position
        Me.len = Length
    End Sub
End Class
Public Class UnAccessableByte
    Inherits AttributeByte
    Friend hb As HexBox
    Friend Sub New(ByVal hb As HexBox)
        Me.hb = hb
    End Sub
    Public Overrides Sub AddRange(ByVal position As Long, ByVal length As Long)
        MyBase.AddRange(position, length)
        hb.InvalidatePOS(position, length)
    End Sub
    Public Overrides Sub RemoveRange(ByVal position As Long, ByVal length As Long)
        MyBase.RemoveRange(position, length)
        hb.InvalidatePOS(position, length)
    End Sub
    Public Overrides Sub Clear()
        MyBase.Clear()
        hb.Invalidate()
    End Sub
End Class
Public Class UnEditableByte
    Inherits AttributeByte
    Friend hb As HexBox
    Friend Sub New(ByVal hb As HexBox)
        Me.hb = hb
    End Sub
    Public Overrides Sub AddRange(ByVal position As Long, ByVal length As Long)
        MyBase.AddRange(position, length)
        hb.InvalidatePOS(position, length)
    End Sub
    Public Overrides Sub RemoveRange(ByVal position As Long, ByVal length As Long)
        MyBase.RemoveRange(position, length)
        hb.InvalidatePOS(position, length)
    End Sub
    Public Overrides Sub Clear()
        MyBase.Clear()
        hb.Invalidate()
    End Sub
End Class
Friend Class WrongFormatted
    Public Box As BoxItem
    Public Position As Long 'bytes
    Public Text As String

    Public Sub New(Box As BoxItem, Position As Long, Text As String)
        Me.Box = Box
        Me.Position = Position
        Me.Text = Text
    End Sub
End Class
