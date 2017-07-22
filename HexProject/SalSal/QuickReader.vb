Friend Class QuickReader
    Public MaxLength As Integer = 1024
    Public Buffer(MaxLength - 1) As Byte
    Public Length As Integer
    Public Position As Long
    Public bb As ByteBuilder

    Public UniqueSession As Integer

    Public Sub New(ByVal bb As ByteBuilder)
        Me.bb = bb
    End Sub
    Public Sub New(ByVal bb As ByteBuilder, ByVal MaxLength As Integer)
        Me.bb = bb
        Me.MaxLength = MaxLength
        ReDim Buffer(MaxLength - 1)
    End Sub
    Public Function Update(ByVal position As Long, ByVal length As Integer) As Integer
        Dim afterLength As Long = bb.GetLength
        If position + length > afterLength Then
            length = afterLength - position
            If length < 0 Then length = 0
        End If
        Dim needUpdate As Boolean = False
        If Not (Me.Position <= position And position + length <= Me.Position + Me.Length) Then
            needUpdate = True
        End If
        If Me.UniqueSession = 0 OrElse Not Me.UniqueSession = bb.UniqueSession Then
            needUpdate = True
        End If

        If position >= afterLength Then
            Me.Position = position
            Me.Length = 0
            Return 0
        End If

        If Not needUpdate Then Return length

        Dim transPosition As Long = position
        Dim theLength As Integer = Me.MaxLength
        If length > Me.MaxLength Then
            theLength = length
            ReDim Buffer(length - 1)
        Else
            Dim sisa As Integer = Me.MaxLength - length
            Dim diffBottom As Integer = Math.Floor(sisa / 2)
            Dim diffTop As Integer = sisa - diffBottom
            transPosition = position - diffBottom
            If transPosition < 0 Then
                diffTop -= transPosition
                transPosition = 0
            End If
        End If

        Dim rlen As Integer = bb.Read(Buffer, transPosition, 0, theLength)
        Me.Position = transPosition
        Me.UniqueSession = bb.UniqueSession
        Me.Length = rlen
        If rlen - (position - transPosition) >= length Then
            Return length
        Else
            Return rlen - (position - transPosition)
        End If
    End Function
End Class
