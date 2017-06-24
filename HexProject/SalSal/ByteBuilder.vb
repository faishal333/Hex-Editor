Imports System.IO
Imports System.Reflection
Imports System.Windows.Forms

Public Class ByteBuilder
    Implements IDisposable

    Friend source As IO.Stream
    Friend store As IO.Stream
    Friend isUserBufferStream As Boolean

    Friend UniqueSession As Integer
    Friend baseAction As ActionRaw
    Friend ActionRaws As New List(Of ActionRaw)
    Friend ActionMods As New List(Of ActionModified)
    Friend lastUpdateActionIndex As Integer

    Friend WithEvents um As New UndoableManager

    Public Event Load(ByVal sender As Object, ByVal e As EventArgs)
    Public Event ByteChanged(ByVal sender As Object, ByVal e As ByteChangedEventArgs)
    Public Event LoadUndoableState(ByVal sender As Object, ByVal Item As UndoableByteBuilderEventArgs)
    Public Event NewUndoableState(ByVal sender As Object, ByVal Item As UndoableByteBuilderEventArgs)

    Public ReadOnly Property BaseStream As IO.Stream
        Get
            Return source
        End Get
    End Property
    Public ReadOnly Property BufferStream As IO.Stream
        Get
            Return store
        End Get
    End Property
    Public ReadOnly Property Length As Long
        Get
            Return GetLength()
        End Get
    End Property
    Public ReadOnly Property Undoable As UndoableManager
        Get
            Return um
        End Get
    End Property
    Public Sub New(ByVal stream As IO.Stream)
        Me.Initialize(stream)
    End Sub

    Public Sub LoadStream(ByVal stream As IO.Stream)
        Me.Initialize(stream)
    End Sub

    Friend Sub Initialize(ByVal stream As IO.Stream)
        um.Clear()
        Me.Clear()
        If Not IsNothing(store) Then
            store.SetLength(0)
        End If

        Me.source = stream

        Dim act As New ActionRaw
        act.Type = ActionTypes.Insert
        act.Position = 0
        act.Length = source.Length
        act.Unique = GenerateUniqueID()
        act.IsStreamed = True
        act.StreamPosition = 0
        act.SourceStream = source

        baseAction = act
        ActionRaws.Add(baseAction)
        UniqueSession = act.Unique

        AddUndoableState()

        RaiseEvent Load(Me, New EventArgs)
    End Sub

    Friend Sub Clear()
        ActionRaws.Clear()
        ActionMods.Clear()
        lastUpdateActionIndex = 0
    End Sub
    Friend Sub Reset()
        Clear()
        Dim act As New ActionRaw
        act.Type = ActionTypes.Insert
        act.Position = 0
        act.Length = source.Length
        act.Unique = GenerateUniqueID()
        act.IsStreamed = True
        act.StreamPosition = 0
        act.SourceStream = source

        baseAction = act
        ActionRaws.Add(baseAction)
    End Sub

    Friend Function GenerateUniqueID() As Integer
        Dim id As Integer = BitConverter.ToInt32(Guid.NewGuid.ToByteArray, 0)
        Return id
    End Function

    Friend Sub _over(ByVal position As Long, ByVal data As Byte(), ByVal format As ModifyFormats)
        If IsNothing(data) Then Exit Sub
        If data.Length = 0 Then Exit Sub
        If position < 0 Then Exit Sub

        Dim theMaxLength As Long = GetLength()

        If position >= theMaxLength Then Exit Sub

        Dim act As New ActionRaw
        act.Type = ActionTypes.Over
        act.Position = position
        act.Length = data.Length
        act.Unique = GenerateUniqueID()

        If format.DataStore = DataStores.Memory Then
            act.Data = data
        ElseIf format.DataStore = DataStores.External Then
            act.Data = Nothing
            act.IsStreamed = True
            act.SourceStream = store
            act.StreamPosition = AppendStore(data)
        End If

        ActionRaws.Add(act)

        UniqueSession = act.Unique
    End Sub
    Friend Sub _over(ByVal position As Long, ByVal data As ByteStreamSource, ByVal format As ModifyFormats)
        If IsNothing(data) Then Exit Sub
        If data.Length = 0 Then Exit Sub
        If position < 0 Then Exit Sub

        Dim theMaxLength As Long = GetLength()

        If position >= theMaxLength Then Exit Sub

        Dim act As New ActionRaw
        act.Type = ActionTypes.Over
        act.Position = position
        act.Length = data.Length
        act.Unique = GenerateUniqueID()

        act.Data = Nothing
        act.IsStreamed = True
        act.SourceStream = data.Stream
        act.StreamPosition = data.Position

        ActionRaws.Add(act)

        UniqueSession = act.Unique
    End Sub
    Friend Sub _insert(ByVal position As Long, ByVal data As Byte(), ByVal format As ModifyFormats)
        If IsNothing(data) Then Exit Sub
        If data.Length = 0 Then Exit Sub
        If position < 0 Then Exit Sub

        Dim theMaxLength As Long = GetLength()

        If position > theMaxLength Then Exit Sub

        Dim act As New ActionRaw
        act.Type = ActionTypes.Insert
        act.Position = position
        act.Length = data.Length
        act.Unique = GenerateUniqueID()
        If format.DataStore = DataStores.Memory Then
            act.Data = data
        ElseIf format.DataStore = DataStores.External Then
            act.Data = Nothing
            act.IsStreamed = True
            act.SourceStream = store
            act.StreamPosition = AppendStore(data)
        End If

        ActionRaws.Add(act)

        UniqueSession = act.Unique
    End Sub
    Friend Sub _insert(ByVal position As Long, ByVal data As ByteStreamSource, ByVal format As ModifyFormats)
        If IsNothing(data) Then Exit Sub
        If data.Length = 0 Then Exit Sub
        If position < 0 Then Exit Sub

        Dim theMaxLength As Long = GetLength()

        If position > theMaxLength Then Exit Sub

        Dim act As New ActionRaw
        act.Type = ActionTypes.Insert
        act.Position = position
        act.Length = data.Length
        act.Unique = GenerateUniqueID()

        act.IsStreamed = True
        act.SourceStream = data.Stream
        act.StreamPosition = data.Position

        ActionRaws.Add(act)

        UniqueSession = act.Unique
    End Sub
    Friend Sub _delete(ByVal position As Long, ByVal length As Long, ByVal format As ModifyFormats)
        If length < 1 Then Exit Sub
        If position < 0 Then Exit Sub

        Dim theMaxLength As Long = GetLength()

        If position >= theMaxLength Then Exit Sub
        If position < theMaxLength And theMaxLength < position + length Then
            length = theMaxLength - position
        End If

        Dim act As New ActionRaw
        act.Type = ActionTypes.Remove
        act.Position = position
        act.Length = length
        act.Unique = GenerateUniqueID()

        ActionRaws.Add(act)

        UniqueSession = act.Unique
    End Sub

    Public Function GetLength() As Long
        Dim theMaxLength As Long = 0

        For Each i In ActionRaws
            Select Case i.Type
                Case ActionTypes.Over
                Case ActionTypes.Insert
                    theMaxLength += i.Length
                Case ActionTypes.Remove
                    theMaxLength -= i.Length
            End Select
        Next

        Return theMaxLength
    End Function
    Public Function Read(ByVal Buffer As Byte(), ByVal position As Long, ByVal offset As Long, ByVal length As Integer) As Integer
        If length < 1 Then
            Return 0
        End If

        Dim actL As Integer = ActionRaws.Count

        Dim ac As ActionModified = Nothing
        Dim act As ActionRaw = Nothing

        Dim theMaxLength As Long = GetLength()
        Dim needUpdate As Boolean = False

        If lastUpdateActionIndex < actL Then
            Dim startUpdate As Integer = 0
            Dim ixx As Integer = lastUpdateActionIndex
            If ixx = 0 Then ixx = 1
            Dim theOriginal As ActionRaw = ActionRaws(ixx - 1)
            For i As Integer = 0 To ActionMods.Count - 1
                If ActionMods(i).Original Is theOriginal Then
                    startUpdate = i + 1
                End If
            Next

            For i As Integer = lastUpdateActionIndex To actL - 1
                act = ActionRaws(i)
                ac = New ActionModified
                ac.Type = act.Type
                ac.Position = act.Position
                ac.Length = act.Length
                ac.Data = act.Data
                ac.Offset = 0
                ac.IsStreamed = act.IsStreamed
                ac.StreamPosition = act.StreamPosition
                ac.SourceStream = act.SourceStream

                ac.Original = act
                ac.Unique = act.Unique
                ActionMods.Add(ac)
            Next

            Dim testOri As Long = 0
            Dim testCp As Long = 0
            For Each i In ActionRaws
                testOri += i.Unique
            Next
            For Each i In ActionMods
                testCp += i.Unique
            Next

            If Not testOri = testCp Then
                ActionMods.Clear()
                For i As Integer = 0 To actL - 1
                    act = ActionRaws(i)
                    ac = New ActionModified
                    ac.Type = act.Type
                    ac.Position = act.Position
                    ac.Length = act.Length
                    ac.Data = act.Data
                    ac.Offset = 0
                    ac.IsStreamed = act.IsStreamed
                    ac.StreamPosition = act.StreamPosition
                    ac.SourceStream = act.SourceStream

                    ac.Original = act
                    ac.Unique = act.Unique
                    ActionMods.Add(ac)
                Next
            End If

            Dim ac2 As ActionModified = Nothing
            Dim seekOffset As Long = 0
            Dim ix As Integer = startUpdate
            Dim iL As Integer = ActionMods.Count

            Do
                ac = ActionMods(ix)
                If Not ac.Length = 0 Then
                    Select Case ac.Type
                        Case ActionTypes.Over
                            seekOffset = 0
                        Case ActionTypes.Insert
                            seekOffset = ac.Length
                        Case ActionTypes.Remove
                            seekOffset = -ac.Length
                    End Select

                    For i2 As Integer = 0 To ix - 1
                        ac2 = ActionMods(i2)
                        If Not ac2.Length = 0 Then
                            If ac.Position <= ac2.Position And ac.Position + ac.Length < ac2.Position Then
                                ac2.Position += seekOffset
                            ElseIf ac.Position <= ac2.Position And ac2.Position < ac.Position + ac.Length Then
                                If ac.Type = ActionTypes.Insert Then
                                    ac2.Position += seekOffset
                                ElseIf ac.Type = ActionTypes.Remove Then
                                    Dim deleted As Long = (ac.Position + ac.Length) - ac2.Position
                                    If deleted > ac2.Length Then deleted = ac2.Length
                                    Dim ac3newLength As Long = ac2.Length - deleted
                                    ac2.Length = 0

                                    If ac3newLength > 0 Then
                                        Dim ac3position As Long = ac.Position
                                        Dim ac3offset As Long = deleted
                                        ac2.Position = ac3position
                                        ac2.Length = ac3newLength
                                        ac2.Data = ac2.Data
                                        ac2.Offset += ac3offset
                                    End If
                                ElseIf ac.Type = ActionTypes.Over Then
                                    Dim deleted As Long = (ac.Position + ac.Length) - ac2.Position
                                    If deleted > ac2.Length Then deleted = ac2.Length
                                    Dim ac3newLength As Long = ac2.Length - deleted
                                    ac2.Length = 0
                                    If ac3newLength > 0 Then
                                        Dim ac3position As Long = ac2.Position + deleted
                                        Dim ac3offset As Long = deleted
                                        ac2.Position = ac3position
                                        ac2.Length = ac3newLength
                                        ac2.Data = ac2.Data
                                        ac2.Offset += ac3offset
                                    End If
                                End If
                            ElseIf ac2.Position <= ac.Position And ac.Position < ac2.Position + ac2.Length Then
                                If ac.Type = ActionTypes.Insert Then
                                    Dim ac2newLength As Long = ac.Position - ac2.Position
                                    Dim ac3newLength As Long = ac2.Length - ac2newLength
                                    Dim ac3position As Long = (ac.Position + ac.Length)
                                    Dim ac3offset As Long = ac2newLength + ac2.Offset
                                    ac2.Length = ac2newLength
                                    Dim ac3 As New ActionModified
                                    ac3.Type = ac2.Type
                                    ac3.Position = ac3position
                                    ac3.Length = ac3newLength
                                    ac3.Data = ac2.Data
                                    ac3.Offset = ac3offset
                                    ac3.Original = ac.Original
                                    ac3.Unique = 1
                                    ac2.Unique -= 1
                                    ac3.IsStreamed = ac2.IsStreamed
                                    ac3.StreamPosition = ac2.StreamPosition
                                    ac3.SourceStream = ac2.SourceStream
                                    ActionMods.Insert(ix + 1, ac3)
                                    iL += 1
                                    ix += 1
                                ElseIf ac.Type = ActionTypes.Remove Then
                                    Dim ac2newLength As Long = ac.Position - ac2.Position
                                    Dim ac3newLength As Long = ac2.Length - ac2newLength - ac.Length
                                    Dim ac3position As Long = ac.Position
                                    Dim ac3offset As Long = (ac2newLength + ac.Length) + ac2.Offset
                                    ac2.Length = ac2newLength

                                    If ac3newLength > 0 Then
                                        Dim ac3 As New ActionModified
                                        ac3.Type = ac2.Type
                                        ac3.Position = ac3position
                                        ac3.Length = ac3newLength
                                        ac3.Data = ac2.Data
                                        ac3.Offset = ac3offset
                                        ac3.Original = ac.Original
                                        ac3.Unique = 1
                                        ac2.Unique -= 1
                                        ac3.IsStreamed = ac2.IsStreamed
                                        ac3.StreamPosition = ac2.StreamPosition
                                        ac3.SourceStream = ac2.SourceStream
                                        ActionMods.Insert(ix + 1, ac3)
                                        iL += 1
                                        ix += 1
                                    End If
                                ElseIf ac.Type = ActionTypes.Over Then
                                    Dim ac2newLength As Long = ac.Position - ac2.Position
                                    Dim ac3newLength As Long = ac2.Length - ac2newLength - ac.Length
                                    Dim ac3position As Long = ac.Position + ac.Length
                                    Dim ac3offset As Long = (ac2newLength + ac.Length) + ac2.Offset
                                    ac2.Length = ac2newLength
                                    If ac3newLength > 0 Then
                                        Dim ac3 As New ActionModified
                                        ac3.Type = ac2.Type
                                        ac3.Position = ac3position
                                        ac3.Length = ac3newLength
                                        ac3.Data = ac2.Data
                                        ac3.Offset = ac3offset
                                        'last edit ac2.original to ac.original
                                        ac3.Original = ac.Original
                                        ac3.Unique = 1
                                        ac2.Unique -= 1
                                        ac3.IsStreamed = ac2.IsStreamed
                                        ac3.StreamPosition = ac2.StreamPosition
                                        ac3.SourceStream = ac2.SourceStream
                                        ActionMods.Insert(ix + 1, ac3)
                                        iL += 1
                                        ix += 1
                                    End If
                                End If
                            ElseIf ac.Position + ac.Length <= ac2.Position Then
                                ac2.Position += seekOffset
                            Else
                            End If
                        End If
                    Next
                End If
                ix += 1
            Loop Until ix >= iL

            needUpdate = True
        End If

        Dim theLength As Long = 0
        If theMaxLength > Buffer.Length Then theMaxLength = Buffer.Length
        If length > theMaxLength Then length = theMaxLength

        For i As Integer = 0 To ActionMods.Count - 1
            ac = ActionMods(i)
            If Not ac.Length = 0 Then
                If ac.Type = ActionTypes.Remove Then
                Else
                    If position <= ac.Position And ac.Position < position + length Then
                        Dim offsetd As Long = ac.Position - position
                        Dim countd As Long = length
                        If offsetd + countd >= ac.Length Then countd = ac.Length - offsetd
                        ' If theLength + countd >= Buffer.Length Then countd = Buffer.Length - offsetd
                        If countd < 0 Then countd = 0

                        If ac.IsStreamed And Not countd = 0 Then
                            ac.SourceStream.Position = ac.StreamPosition + ac.Offset
                            ac.SourceStream.Read(Buffer, offset + offsetd + theLength, countd)
                        Else
                            For i2 As Integer = 0 To countd - 1
                                Buffer(i2 + offset + offsetd + theLength) = ac.Data(i2 + ac.Offset)
                            Next
                        End If

                        theLength += countd
                        position += countd
                    ElseIf (position <= (ac.Position + ac.Length) And (ac.Position + ac.Length) < (position + length)) Or (ac.Position < position And position + length < ac.Position + ac.Length) Then
                        Dim offsetd As Long = position - ac.Position
                        Dim countd As Long = length
                        If offsetd + countd >= ac.Length Then countd = ac.Length - offsetd
                        'If theLength + countd >= Buffer.Length Then countd = Buffer.Length - offsetd
                        If countd < 0 Then countd = 0

                        If ac.IsStreamed And Not countd = 0 Then
                            ac.SourceStream.Position = ac.StreamPosition + offsetd + ac.Offset
                            ac.SourceStream.Read(Buffer, offset + theLength, countd)
                        Else
                            For i2 As Integer = 0 To countd - 1
                                Buffer(i2 + offset + theLength) = ac.Data(i2 + offsetd + ac.Offset)
                            Next
                        End If
                        theLength += countd
                        position += countd
                    End If
                End If
            End If
        Next

        lastUpdateActionIndex = ActionRaws.Count
        Return theLength
    End Function
    Friend Function Read2(ByVal Buffer As Byte(), ByVal position As Long, ByVal offset As Long, ByVal length As Integer, ByVal ActionRaws As List(Of ActionRaw), ByVal max As Integer) As Integer
        If length < 1 Then
            Return 0
        End If

        Dim lastUpdateActionIndex As Integer = 0
        Dim actL As Integer = max

        Dim ac As ActionModified = Nothing
        Dim act As ActionRaw = Nothing
        Dim ActionMods As New List(Of ActionModified)

        Dim theMaxLength As Long = GetLength()
        Dim needUpdate As Boolean = False

        If lastUpdateActionIndex < actL Then
            Dim startUpdate As Integer = 0
            Dim ixx As Integer = lastUpdateActionIndex
            If ixx = 0 Then ixx = 1
            Dim theOriginal As ActionRaw = ActionRaws(ixx - 1)
            For i As Integer = 0 To ActionMods.Count - 1
                If ActionMods(i).Original Is theOriginal Then
                    startUpdate = i + 1
                End If
            Next

            For i As Integer = lastUpdateActionIndex To actL - 1
                act = ActionRaws(i)
                ac = New ActionModified
                ac.Type = act.Type
                ac.Position = act.Position
                ac.Length = act.Length
                ac.Data = act.Data
                ac.Offset = 0
                ac.IsStreamed = act.IsStreamed
                ac.StreamPosition = act.StreamPosition
                ac.SourceStream = act.SourceStream

                ac.Original = act
                ac.Unique = act.Unique
                ActionMods.Add(ac)
            Next

            Dim testOri As Long = 0
            Dim testCp As Long = 0
            For Each i In ActionRaws
                testOri += i.Unique
            Next
            For Each i In ActionMods
                testCp += i.Unique
            Next

            If Not testOri = testCp Then
                ActionMods.Clear()
                For i As Integer = 0 To actL - 1
                    act = ActionRaws(i)
                    ac = New ActionModified
                    ac.Type = act.Type
                    ac.Position = act.Position
                    ac.Length = act.Length
                    ac.Data = act.Data
                    ac.Offset = 0
                    ac.IsStreamed = act.IsStreamed
                    ac.StreamPosition = act.StreamPosition
                    ac.SourceStream = act.SourceStream

                    ac.Original = act
                    ac.Unique = act.Unique
                    ActionMods.Add(ac)
                Next
            End If

            Dim ac2 As ActionModified = Nothing
            Dim seekOffset As Long = 0
            Dim ix As Integer = startUpdate
            Dim iL As Integer = ActionMods.Count

            Do
                ac = ActionMods(ix)
                If Not ac.Length = 0 Then
                    Select Case ac.Type
                        Case ActionTypes.Over
                            seekOffset = 0
                        Case ActionTypes.Insert
                            seekOffset = ac.Length
                        Case ActionTypes.Remove
                            seekOffset = -ac.Length
                    End Select

                    For i2 As Integer = 0 To ix - 1
                        ac2 = ActionMods(i2)
                        If Not ac2.Length = 0 Then
                            If ac.Position <= ac2.Position And ac.Position + ac.Length < ac2.Position Then
                                ac2.Position += seekOffset
                            ElseIf ac.Position <= ac2.Position And ac2.Position < ac.Position + ac.Length Then
                                If ac.Type = ActionTypes.Insert Then
                                    ac2.Position += seekOffset
                                ElseIf ac.Type = ActionTypes.Remove Then
                                    Dim deleted As Long = (ac.Position + ac.Length) - ac2.Position
                                    If deleted > ac2.Length Then deleted = ac2.Length
                                    Dim ac3newLength As Long = ac2.Length - deleted
                                    ac2.Length = 0

                                    If ac3newLength > 0 Then
                                        Dim ac3position As Long = ac.Position
                                        Dim ac3offset As Long = deleted
                                        ac2.Position = ac3position
                                        ac2.Length = ac3newLength
                                        ac2.Data = ac2.Data
                                        ac2.Offset += ac3offset
                                    End If
                                ElseIf ac.Type = ActionTypes.Over Then
                                    Dim deleted As Long = (ac.Position + ac.Length) - ac2.Position
                                    If deleted > ac2.Length Then deleted = ac2.Length
                                    Dim ac3newLength As Long = ac2.Length - deleted
                                    ac2.Length = 0
                                    If ac3newLength > 0 Then
                                        Dim ac3position As Long = ac2.Position + deleted
                                        Dim ac3offset As Long = deleted
                                        ac2.Position = ac3position
                                        ac2.Length = ac3newLength
                                        ac2.Data = ac2.Data
                                        ac2.Offset += ac3offset
                                    End If
                                End If
                            ElseIf ac2.Position <= ac.Position And ac.Position < ac2.Position + ac2.Length Then
                                If ac.Type = ActionTypes.Insert Then
                                    Dim ac2newLength As Long = ac.Position - ac2.Position
                                    Dim ac3newLength As Long = ac2.Length - ac2newLength
                                    Dim ac3position As Long = (ac.Position + ac.Length)
                                    Dim ac3offset As Long = ac2newLength + ac2.Offset
                                    ac2.Length = ac2newLength
                                    Dim ac3 As New ActionModified
                                    ac3.Type = ac2.Type
                                    ac3.Position = ac3position
                                    ac3.Length = ac3newLength
                                    ac3.Data = ac2.Data
                                    ac3.Offset = ac3offset
                                    ac3.Original = ac.Original
                                    ac3.Unique = 1
                                    ac2.Unique -= 1
                                    ac3.IsStreamed = ac2.IsStreamed
                                    ac3.StreamPosition = ac2.StreamPosition
                                    ac3.SourceStream = ac2.SourceStream
                                    ActionMods.Insert(ix + 1, ac3)
                                    iL += 1
                                    ix += 1
                                ElseIf ac.Type = ActionTypes.Remove Then
                                    Dim ac2newLength As Long = ac.Position - ac2.Position
                                    Dim ac3newLength As Long = ac2.Length - ac2newLength - ac.Length
                                    Dim ac3position As Long = ac.Position
                                    Dim ac3offset As Long = (ac2newLength + ac.Length) + ac2.Offset
                                    ac2.Length = ac2newLength

                                    If ac3newLength > 0 Then
                                        Dim ac3 As New ActionModified
                                        ac3.Type = ac2.Type
                                        ac3.Position = ac3position
                                        ac3.Length = ac3newLength
                                        ac3.Data = ac2.Data
                                        ac3.Offset = ac3offset
                                        ac3.Original = ac.Original
                                        ac3.Unique = 1
                                        ac2.Unique -= 1
                                        ac3.IsStreamed = ac2.IsStreamed
                                        ac3.StreamPosition = ac2.StreamPosition
                                        ac3.SourceStream = ac2.SourceStream
                                        ActionMods.Insert(ix + 1, ac3)
                                        iL += 1
                                        ix += 1
                                    End If
                                ElseIf ac.Type = ActionTypes.Over Then
                                    Dim ac2newLength As Long = ac.Position - ac2.Position
                                    Dim ac3newLength As Long = ac2.Length - ac2newLength - ac.Length
                                    Dim ac3position As Long = ac.Position + ac.Length
                                    Dim ac3offset As Long = (ac2newLength + ac.Length) + ac2.Offset
                                    ac2.Length = ac2newLength
                                    If ac3newLength > 0 Then
                                        Dim ac3 As New ActionModified
                                        ac3.Type = ac2.Type
                                        ac3.Position = ac3position
                                        ac3.Length = ac3newLength
                                        ac3.Data = ac2.Data
                                        ac3.Offset = ac3offset
                                        'last edit ac2.original to ac.original
                                        ac3.Original = ac.Original
                                        ac3.Unique = 1
                                        ac2.Unique -= 1
                                        ac3.IsStreamed = ac2.IsStreamed
                                        ac3.StreamPosition = ac2.StreamPosition
                                        ac3.SourceStream = ac2.SourceStream
                                        ActionMods.Insert(ix + 1, ac3)
                                        iL += 1
                                        ix += 1
                                    End If
                                End If
                            ElseIf ac.Position + ac.Length <= ac2.Position Then
                                ac2.Position += seekOffset
                            Else
                            End If
                        End If
                    Next
                End If
                ix += 1
            Loop Until ix >= iL

            needUpdate = True
        End If

        Dim theLength As Long = 0
        If theMaxLength > Buffer.Length Then theMaxLength = Buffer.Length
        If length > theMaxLength Then length = theMaxLength

        For i As Integer = 0 To ActionMods.Count - 1
            ac = ActionMods(i)
            If Not ac.Length = 0 Then
                If ac.Type = ActionTypes.Remove Then
                Else
                    If position <= ac.Position And ac.Position < position + length Then
                        Dim offsetd As Long = ac.Position - position
                        Dim countd As Long = theMaxLength - offsetd
                        If countd >= ac.Length Then countd = ac.Length
                        If countd < 0 Then countd = 0

                        If ac.IsStreamed And Not countd = 0 Then
                            ac.SourceStream.Position = ac.StreamPosition + ac.Offset
                            ac.SourceStream.Read(Buffer, offset + offsetd, countd)
                        Else
                            For i2 As Integer = 0 To countd - 1
                                Buffer(i2 + offset + offsetd) = ac.Data(i2 + ac.Offset)
                            Next
                        End If

                        theLength += countd
                    ElseIf (position <= (ac.Position + ac.Length) And (ac.Position + ac.Length) < (position + length)) Or (ac.Position < position And position + length < ac.Position + ac.Length) Then
                        Dim offsetd As Long = position - ac.Position
                        Dim countd As Long = theMaxLength - offsetd
                        If offsetd + countd >= ac.Length Then countd = ac.Length - offsetd
                        If countd < 0 Then countd = 0

                        If ac.IsStreamed And Not countd = 0 Then
                            ac.SourceStream.Position = ac.StreamPosition + offsetd + ac.Offset
                            ac.SourceStream.Read(Buffer, offset, countd)
                        Else
                            For i2 As Integer = 0 To countd - 1
                                Buffer(i2 + offset) = ac.Data(i2 + offsetd + ac.Offset)
                            Next
                        End If
                        theLength += countd
                    End If
                End If
            End If
        Next

        Return theLength
    End Function

#Region "Modifier"
    Public Function Write(ByVal buffer As Byte(), ByVal position As Long, ByVal offset As Integer, ByVal length As Integer) As Integer
        Return Me.Write(buffer, position, offset, length, New ModifyFormats)
    End Function
    Public Function Write(ByVal buffer As Byte(), ByVal position As Long, ByVal offset As Integer, ByVal length As Integer, ByVal format As ModifyFormats) As Integer
        If position < 0 Then Return 0
        If length < 1 Then Return 0

        Dim theWriteLength As Integer = 0
        Dim theMaxLength As Long = GetLength()

        If IsNothing(format) Then
            format = DefaultModifyFormat
        End If

        Dim bc As ByteChangedEventArgs = Nothing
        If position + length <= theMaxLength Then
            Dim data(length - 1) As Byte
            For i As Integer = 0 To length - 1
                data(i) = buffer(i + offset)
            Next

            Me._over(position, data, format)
            theWriteLength = length

            If format.EnableUpdateEventHandler Then bc = New ByteChangedEventArgs(position, theWriteLength, ActionTypes.Over)
        ElseIf position <= theMaxLength And theMaxLength < position + length Then
            Dim overCount As Integer = theMaxLength - position

            If Not overCount = 0 Then
                Dim data(overCount - 1) As Byte
                For i As Integer = 0 To overCount - 1
                    data(i) = buffer(i + offset)
                Next
                Me._over(position, data, format)
                offset += overCount
            End If

            Dim insertCount As Integer = length - overCount
            Dim data2(insertCount - 1) As Byte
            For i As Integer = 0 To insertCount - 1
                data2(i) = buffer(i + offset)
            Next
            Me._insert(position + overCount, data2, format)
            theWriteLength = length

            If format.EnableUpdateEventHandler Then bc = New ByteChangedEventArgs(position, theWriteLength, ActionTypes.Over Or ActionTypes.Insert)
        Else
            Dim dbuffLen As Integer = UShort.MaxValue
            Dim dbuff() As Byte = Nothing
            If position - theMaxLength >= dbuffLen Then
                ReDim dbuff(dbuffLen - 1)
            End If

            For i As Long = theMaxLength To position - 1 Step UShort.MaxValue
                If position - i < dbuffLen Then
                    dbuffLen = position - i
                    Dim dbuff2(dbuffLen - 1) As Byte
                    Me._insert(i, dbuff2, format)
                Else
                    Me._insert(i, dbuff, format)
                End If
            Next

            Dim data2(length - 1) As Byte
            For i As Integer = 0 To length - 1
                data2(i) = buffer(i + offset)
            Next
            Me._insert(position, data2, format)
            theWriteLength = length

            If format.EnableUpdateEventHandler Then bc = New ByteChangedEventArgs(position, theWriteLength, ActionTypes.Over Or ActionTypes.Insert)
        End If

        AddUndoableState(format.UndoableData)

        If format.EnableUpdateEventHandler Then RaiseEvent ByteChanged(Me, New ByteChangedEventArgs(0, GetLength, ActionTypes.Insert))
        Return theWriteLength
    End Function
    Public Sub Over(ByVal position As Long, ByVal data As Byte())
        Me.Over(position, data, DefaultModifyFormat)
    End Sub
    Public Sub Over(ByVal position As Long, ByVal data As Byte(), ByVal format As ModifyFormats)
        If IsNothing(format) Then
            format = DefaultModifyFormat
        End If

        Dim afterLen As Long = GetLength()

        _over(position, data, format)

        Dim dataLen As Integer = data.Length
        If position + data.Length > afterLen Then
            dataLen = afterLen - position
        End If

        AddUndoableState(format.UndoableData)

        If format.EnableUpdateEventHandler Then RaiseEvent ByteChanged(Me, New ByteChangedEventArgs(position, dataLen, ActionTypes.Over))
    End Sub
    Public Sub Over(ByVal position As Long, ByVal data As ByteStreamSource)
        Me.Over(position, data, DefaultModifyFormat)
    End Sub
    Public Sub Over(ByVal position As Long, ByVal data As ByteStreamSource, ByVal format As ModifyFormats)
        If IsNothing(format) Then
            format = DefaultModifyFormat
        End If

        Dim afterLen As Long = GetLength()

        If format.DirectCopy Then
            Dim p As Long = AppendStore(data)
            data = New ByteStreamSource(store, p, data.Length)
        End If

        _over(position, data, format)

        Dim dataLen As Integer = data.Length
        If position + data.Length > afterLen Then
            dataLen = afterLen - position
        End If

        AddUndoableState(format.UndoableData)

        If format.EnableUpdateEventHandler Then RaiseEvent ByteChanged(Me, New ByteChangedEventArgs(position, dataLen, ActionTypes.Over))
    End Sub
    Public Sub Insert(ByVal position As Long, ByVal data As Byte())
        Me.Insert(position, data, DefaultModifyFormat)
    End Sub
    Public Sub Insert(ByVal position As Long, ByVal data As Byte(), ByVal format As ModifyFormats)
        If IsNothing(format) Then
            format = DefaultModifyFormat
        End If

        _insert(position, data, format)

        AddUndoableState(format.UndoableData)

        If format.EnableUpdateEventHandler Then RaiseEvent ByteChanged(Me, New ByteChangedEventArgs(position, data.Length, ActionTypes.Insert))
    End Sub
    Public Sub Insert(ByVal position As Long, ByVal data As ByteStreamSource)
        Me.Insert(position, data, DefaultModifyFormat)
    End Sub
    Public Sub Insert(ByVal position As Long, ByVal data As ByteStreamSource, ByVal format As ModifyFormats)
        If IsNothing(format) Then
            format = DefaultModifyFormat
        End If

        If format.DirectCopy Then
            Dim p As Long = AppendStore(data)
            data = New ByteStreamSource(store, p, data.Length)
        End If

        _insert(position, data, format)

        AddUndoableState(format.UndoableData)

        If format.EnableUpdateEventHandler Then RaiseEvent ByteChanged(Me, New ByteChangedEventArgs(position, data.Length, ActionTypes.Insert))
    End Sub
    Public Sub Remove(ByVal position As Long, ByVal length As Long)
        Me.Remove(position, length, DefaultModifyFormat)
    End Sub
    Public Sub Remove(ByVal position As Long, ByVal length As Long, ByVal format As ModifyFormats)
        If IsNothing(format) Then
            format = DefaultModifyFormat
        End If

        Dim afterLen As Long = GetLength()

        _delete(position, length, format)

        If position + length > afterLen Then
            length = afterLen - position
        End If

        AddUndoableState(format.UndoableData)

        If format.EnableUpdateEventHandler Then RaiseEvent ByteChanged(Me, New ByteChangedEventArgs(position, length, ActionTypes.Remove))
    End Sub
    Public Sub Append(ByVal data As Byte())
        Me.Append(data, DefaultModifyFormat)
    End Sub
    Public Sub Append(ByVal data As Byte(), ByVal format As ModifyFormats)
        If IsNothing(format) Then
            format = DefaultModifyFormat
        End If

        Dim position As Long = GetLength()
        _insert(position, data, format)

        AddUndoableState(format.UndoableData)

        If format.EnableUpdateEventHandler Then RaiseEvent ByteChanged(Me, New ByteChangedEventArgs(position, data.Length, ActionTypes.Insert))
    End Sub
    Public Sub Append(ByVal data As ByteStreamSource)
        Me.Append(data, DefaultModifyFormat)
    End Sub
    Public Sub Append(ByVal data As ByteStreamSource, ByVal format As ModifyFormats)
        If IsNothing(format) Then
            format = DefaultModifyFormat
        End If

        If format.DirectCopy Then
            Dim p As Long = AppendStore(data)
            data = New ByteStreamSource(store, p, data.Length)
        End If

        Dim position As Long = GetLength()
        _insert(position, data, format)

        AddUndoableState(format.UndoableData)

        If format.EnableUpdateEventHandler Then RaiseEvent ByteChanged(Me, New ByteChangedEventArgs(position, data.Length, ActionTypes.Insert))
    End Sub
    Public Sub SetLength(ByVal newLength As Long)
        SetLength(newLength, DefaultModifyFormat)
    End Sub
    Public Sub SetLength(ByVal newLength As Long, ByVal format As ModifyFormats)
        If IsNothing(format) Then
            format = DefaultModifyFormat
        End If

        Dim afterLength As Long = GetLength()
        If afterLength < newLength Then
            Dim dbuffLen As Integer = UShort.MaxValue
            Dim dbuff() As Byte = Nothing
            If newLength - afterLength >= dbuffLen Then
                ReDim dbuff(dbuffLen - 1)
            End If

            For i As Long = afterLength To newLength - 1 Step UShort.MaxValue
                If newLength - i < dbuffLen Then
                    dbuffLen = newLength - i
                    Dim dbuff2(dbuffLen - 1) As Byte
                    Me._insert(i, dbuff2, format)
                Else
                    Me._insert(i, dbuff, format)
                End If
            Next

            If format.EnableUpdateEventHandler Then RaiseEvent ByteChanged(Me, New ByteChangedEventArgs(afterLength, (newLength - afterLength), ActionTypes.Insert))
        ElseIf afterLength > newLength Then
            Dim len As Long = afterLength - newLength
            _delete(newLength, len, format)

            AddUndoableState(format.UndoableData)

            If format.EnableUpdateEventHandler Then RaiseEvent ByteChanged(Me, New ByteChangedEventArgs(newLength, len, ActionTypes.Remove))
        End If
    End Sub
#End Region

#Region "Save"
    Friend perc As Single
    Public Sub Flush()
        perc = 0
        If ActionRaws.Count = 0 Then
            perc = 100
            Exit Sub
        End If
        Dim fs As Stream = Me.source
        Dim fz As IO.FileStream = TryCast(fs, IO.FileStream)

        Dim fn As String = ""
        If Not IsNothing(fz) Then
            fn = fz.Name
        Else
            fn = New Uri(Assembly.GetExecutingAssembly().CodeBase).AbsolutePath
            fn = IO.Path.GetDirectoryName(fn) & "\temp"
        End If

        If Not fs.CanWrite Or Not fs.CanSeek Then
            Throw New Exception("Cant write on this stream.")
        End If

        Dim reWrite As Boolean = False
        For Each i In ActionRaws
            If Not i.Type = ActionTypes.Over Then
                reWrite = True
                Exit For
            End If
        Next

        If Not reWrite Then
            ReverseOldActions()
            Dim act As ActionRaw = Nothing
            Dim act2 As ActionRaw = Nothing
            Dim l As Integer = ActionRaws.Count - 1
            Dim same As Boolean = False
            For i As Integer = 0 To l
                act = ActionRaws(i)
                same = False
                For i2 As Integer = i + 1 To l
                    act2 = ActionRaws(i2)
                    If act.Position = act2.Position And act.Length = act2.Length And act.Type = act2.Type Then
                        same = True
                        Exit For
                    End If
                Next
                If Not same Then
                    fs.Position = act.Position
                    fs.Write(act.Data, 0, act.Length)
                End If
                perc = i / l * 100
            Next
            Me.Reset()
            perc = 100
        Else
            Dim fnd As Boolean = False
            For i As Integer = 0 To 10000
                If Not IO.File.Exists(fn & i) Then
                    fn = fn & i
                    fnd = True
                    Exit For
                End If
            Next
            If Not fnd Then
                Throw New Exception("Error while saving file.")
            Else
                ReverseOldActions()
                Dim fs2 As New IO.FileStream(fn, IO.FileMode.Create, IO.FileAccess.ReadWrite, IO.FileShare.None)
                Dim afterLength As Long = GetLength()
                Dim buffer(UShort.MaxValue - 1) As Byte
                Dim rlen As Integer = 0
                For i As Long = 0 To afterLength - 1 Step UShort.MaxValue
                    rlen = Read(buffer, i, 0, UShort.MaxValue)
                    fs2.Write(buffer, 0, rlen)
                    perc = i / afterLength * 100
                Next
                Me.Reset()

                If IsNothing(fz) Then
                    fs2.Flush()
                    fs2.Close()
                    fs.Close()
                    IO.File.Delete(fz.Name)
                    IO.File.Move(fn, fz.Name)
                    fs = New IO.FileStream(fz.Name, IO.FileMode.Open, IO.FileAccess.ReadWrite, IO.FileShare.Read)
                    Me.source = fs

                Else
                    fs2.Flush()
                    Dim r As Integer = UShort.MaxValue
                    fs.Position = 0
                    fs2.Position = 0
                    Dim b(r - 1) As Byte
                    For i As Long = 0 To afterLength - 1 Step UShort.MaxValue
                        r = fs2.Read(b, 0, r)
                        If r = 0 Then Exit For
                        fs.Write(b, 0, r)
                    Next
                    fs.SetLength(afterLength)

                    fs2.Close()
                    IO.File.Delete(fs2.Name)
                End If

                perc = 100
            End If
        End If

        baseAction.Length = source.Length
    End Sub

    Friend Sub ReverseOldActions()
        Dim clonedActions As New List(Of ActionRaw)
        Dim act As ActionRaw = Nothing
        Dim act2 As ActionRaw = Nothing
        Dim enableStore As Boolean = CanStore

        For i As Integer = 1 To ActionRaws.Count - 1
            Dim acr As ActionRaw = ActionRaws(i)
            If Not acr.SourceStream Is source Then
                act = New ActionRaw
                act.Type = acr.Type
                act.Position = acr.Position
                act.Length = acr.Length
                act.Data = acr.Data
                act.Unique = acr.Unique
                act.IsStreamed = acr.IsStreamed
                act.SourceStream = acr.SourceStream
                act.StreamPosition = acr.StreamPosition
                clonedActions.Add(act)
            End If
        Next

        Dim l As Integer = clonedActions.Count - 1
        Dim a As Integer = 1
        For Each i In clonedActions
            act2 = i
            Select Case act2.Type
                Case ActionTypes.Over
                    Dim BufferBytes(act2.Length - 1) As Byte
                    Dim rlen As Integer = Read2(BufferBytes, act2.Position, 0, BufferBytes.Length, ActionRaws, a)

                    If act2.IsStreamed Or enableStore Then
                        act2.StreamPosition = AppendStore(BufferBytes)
                        act2.SourceStream = store
                    Else
                        act2.Data = BufferBytes
                    End If
                Case ActionTypes.Insert
                    act2.Type = ActionTypes.Remove
                Case ActionTypes.Remove
                    If act2.IsStreamed Or enableStore Then
                        act2.Type = ActionTypes.Insert

                        Dim fn As Long = act2.Length
                        Dim bn As Integer = 0

                        act2.StreamPosition = store.Length
                        For i2 As Long = 0 To act2.Length - 1
                            If fn > UShort.MaxValue Then
                                bn = UShort.MaxValue
                            Else
                                bn = fn
                            End If

                            Dim BufferBytes(bn - 1) As Byte
                            Dim rlen As Integer = Read2(BufferBytes, act2.Position + i2, 0, bn, ActionRaws, a)

                            If rlen = 0 Then Exit For

                            AppendStore(BufferBytes)
                        Next
                    Else
                        Dim b(UShort.MaxValue - 1) As Byte
                        Dim rlen As Integer = Read2(b, act2.Position, 0, act2.Length, ActionRaws, a)
                        act2.Type = ActionTypes.Insert
                        act2.Data = b
                    End If
            End Select
            a += 1
        Next

        If Not clonedActions.Count = 0 Then
            clonedActions.Reverse()

            For Each i As ByteUndoableItem In um.States
                If i.ActionRaws.Count = 0 Then
                    Throw New Exception
                Else
                    i.ActionRaws.RemoveAt(0)
                    i.ActionRaws.InsertRange(0, clonedActions)
                End If
            Next
        End If
    End Sub

    Public Sub CopyTo(ByVal stream As IO.Stream)
        perc = 0

        Dim afterLength As Long = GetLength()
        Dim buffer(UShort.MaxValue - 1) As Byte
        Dim rlen As Integer = 0
        For i As Long = 0 To afterLength - 1 Step UShort.MaxValue
            rlen = Read(buffer, i, 0, UShort.MaxValue)
            stream.Write(buffer, 0, rlen)
            perc = i / afterLength * 100
        Next

        perc = 100
    End Sub
#End Region

#Region "Undoable"
    Friend Sub AddUndoableState(Optional ByVal data As Object = Nothing)
        Dim ui As New ByteUndoableItem
        ui.ActionRaws = New List(Of ActionRaw)
        ui.ActionRaws.AddRange(ActionRaws)
        ui.Data = data
        um.NewState(ui)

        RaiseEvent NewUndoableState(Me, New UndoableByteBuilderEventArgs(ui))
    End Sub
    Friend Sub um_loadState(ByVal sender As Object, ByVal e As UndoableEventArgs) Handles um.Load
        Dim item As ByteUndoableItem = e.Item
        ActionRaws.Clear()
        ActionMods.Clear()
        lastUpdateActionIndex = 0
        Dim act As New ActionRaw
        act.Type = ActionTypes.Insert
        act.Position = 0
        act.Length = source.Length
        act.Unique = GenerateUniqueID()
        act.IsStreamed = True
        act.StreamPosition = 0
        act.SourceStream = source

        baseAction = act
        ActionRaws.Add(act)
        ActionRaws.AddRange(item.ActionRaws)

        UniqueSession = act.Unique

        RaiseEvent LoadUndoableState(Me, New UndoableByteBuilderEventArgs(e.Item))
        RaiseEvent ByteChanged(Me, New ByteChangedEventArgs(0, GetLength, 0))
    End Sub

#End Region

#Region "Buffer"
    Friend Function AppendStore(ByVal data As Byte()) As Long
        If IsNothing(store) Then
            store = CreateStore()
        End If

        store.Seek(0, IO.SeekOrigin.End)
        Dim position As Long = store.Position
        store.Write(data, 0, data.Length)
        Return position
    End Function
    Friend Function AppendStore(ByVal data As ByteStreamSource) As Long
        If IsNothing(store) Then
            store = CreateStore()
        End If

        store.Seek(0, IO.SeekOrigin.End)
        Dim position As Long = store.Position

        Dim buffLen As Integer = UShort.MaxValue
        If buffLen > data.Length Then
            buffLen = data.Length
        End If
        Dim b(buffLen - 1) As Byte

        data.Stream.Position = data.Position
        Dim totalLength As Long = data.Length
        For i As Long = 0 To totalLength - 1 Step buffLen
            data.Stream.Read(b, 0, buffLen)

            If totalLength < buffLen Then
                buffLen = totalLength
            End If

            store.Write(b, 0, buffLen)
            totalLength -= buffLen
        Next

        Return position
    End Function
    Friend Function AppendStore(ByVal data As Byte(), ByVal length As Integer) As Long
        If IsNothing(store) Then
            store = CreateStore()
        End If

        store.Seek(0, IO.SeekOrigin.End)
        Dim position As Long = store.Position
        store.Write(data, 0, length)
        Return position
    End Function
    Friend Function AppendStore(ByVal data As Byte(), ByVal offset As Integer, ByVal length As Integer) As Long
        If IsNothing(store) Then
            store = CreateStore()
        End If

        store.Seek(0, IO.SeekOrigin.End)
        Dim position As Long = store.Position
        store.Write(data, offset, length)
        Return position
    End Function
    Friend Shared Function CreateStore() As IO.FileStream
        Dim buffPath As String = IO.Path.GetDirectoryName(Application.ExecutablePath) & "\temp\"
        If Not IO.Directory.Exists(buffPath) Then
            IO.Directory.CreateDirectory(buffPath)
        End If

        For i As Integer = 0 To 1000
            If Not IO.File.Exists(buffPath & i) Then
                Return New IO.FileStream(CStr(buffPath & i), IO.FileMode.Create, IO.FileAccess.ReadWrite, FileShare.None)
                Exit For
            End If
        Next
        Return Nothing
    End Function
    Friend Shared Sub DeleteStore(ByVal store As IO.FileStream)
        If IsNothing(store) Then Exit Sub

        If store.CanRead Then
            store.Close()
            IO.File.Delete(store.Name)
        End If
    End Sub
    Public Sub SetBufferStream(ByVal stream As IO.Stream)
        If Not IsNothing(store) Then
            stream.SetLength(0)
            stream.Position = 0
            store.Position = 0
            store.CopyTo(stream)

            Dim oldStore As IO.Stream = store
            store = stream

            If Not isUserBufferStream Then DeleteStore(store)
        Else
            store = stream
        End If
        isUserBufferStream = True
    End Sub
    Friend ReadOnly Property CanStore As Boolean
        Get
            If IsNothing(store) Then Return False
            If Not store.CanWrite Or Not store.CanRead Or Not store.CanSeek Then Return False
            Return True
        End Get
    End Property
    Friend ReadOnly Property DefaultModifyFormat() As ModifyFormats
        Get
            Dim mf As New ModifyFormats
            If CanStore Then
                mf.DataStore = DataStores.External
            End If

            Return mf
        End Get
    End Property
#End Region

#Region "IDisposable Support"
    Private disposedValue As Boolean ' To detect redundant calls

    ' IDisposable
    Protected Overridable Sub Dispose(disposing As Boolean)
        If Not disposedValue Then
            If disposing Then
                ' TODO: dispose managed state (managed objects).
            End If

            ' TODO: free unmanaged resources (unmanaged objects) and override Finalize() below.
            ' TODO: set large fields to null.
            If Not isUserBufferStream Then DeleteStore(store)
            store = Nothing

            Clear()

        End If
        disposedValue = True
    End Sub

    ' TODO: override Finalize() only if Dispose(disposing As Boolean) above has code to free unmanaged resources.
    'Protected Overrides Sub Finalize()
    '    ' Do not change this code.  Put cleanup code in Dispose(disposing As Boolean) above.
    '    Dispose(False)
    '    MyBase.Finalize()
    'End Sub

    ' This code added by Visual Basic to correctly implement the disposable pattern.
    Public Sub Dispose() Implements IDisposable.Dispose
        ' Do not change this code.  Put cleanup code in Dispose(disposing As Boolean) above.
        Dispose(True)
        ' TODO: uncomment the following line if Finalize() is overridden above.
        ' GC.SuppressFinalize(Me)
    End Sub
#End Region

End Class

Public Class ModifyFormats
    Public Property DataStore As DataStores
    Public Property UndoableData As Object
    Public Property EnableUpdateEventHandler As Boolean
    Public Property DirectCopy As Boolean
    Public Sub New()
        Me.DataStore = DataStores.Memory
        Me.EnableUpdateEventHandler = True
    End Sub
    Public Sub New(DataStore As DataStores)
        Me.DataStore = DataStore
        Me.EnableUpdateEventHandler = True
    End Sub
    Public Sub New(DataStore As DataStores, UndoableData As Object)
        Me.DataStore = DataStore
        Me.UndoableData = UndoableData
        Me.EnableUpdateEventHandler = True
    End Sub
End Class

Friend Class ActionRaw
    Public [Type] As ActionTypes
    Public Position As Long
    Public Length As Long
    Public Data As Byte()
    Public Unique As Integer
    Public IsStreamed As Boolean
    Public SourceStream As IO.Stream
    Public StreamPosition As Long
End Class

Friend Class ActionModified
    Public [Type] As ActionTypes
    Public Position As Long
    Public Length As Long
    Public Offset As Long
    Public Data As Byte()
    Public Unique As Integer
    Public IsStreamed As Boolean
    Public SourceStream As IO.Stream
    Public StreamPosition As Long

    Public Original As ActionRaw
End Class

Friend Class ByteUndoableItem
    Inherits UndoableItem
    Friend ActionRaws As New List(Of ActionRaw)
End Class

Public Class UndoableByteBuilderEventArgs
    Inherits EventArgs
    Public ReadOnly Property Item As UndoableItem
    Public Sub New(Item As UndoableItem)
        Me.Item = Item
    End Sub
    Public Sub SetData(ByVal Data As Object)
        Me.Item.Data = Data
    End Sub
    Public Function GetData() As Object
        Return Me.Item.Data
    End Function
End Class

Public Class ByteChangedEventArgs
    Inherits EventArgs
    Public ReadOnly Property Position As Long
    Public ReadOnly Property Length As Long
    Public ReadOnly Property Action As ActionTypes

    Public Sub New(Position As Long, Length As Long, Action As ActionTypes)
        Me.Position = Position
        Me.Length = Length
        Me.Action = Action
    End Sub
End Class

Public Class ByteStreamSource
    Public ReadOnly Property Stream As IO.Stream
    Public ReadOnly Property Position As Long
    Public ReadOnly Property Length As Long
    Public Sub New(stream As IO.Stream, position As Long, length As Long)
        Me.Stream = stream
        Me.Position = position
        Me.Length = length
    End Sub
End Class
Public Enum ActionTypes As Integer
    Over = 1
    Insert = 2
    Remove = 4
End Enum

Public Enum DataStores As Integer
    Memory = 0
    External = 1
End Enum