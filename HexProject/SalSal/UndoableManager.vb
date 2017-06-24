Public Class UndoableManager
    Friend current As UndoableItem
    Friend topLevel As UndoableItem
    Friend States As New List(Of UndoableItem)

    Public Event Load(ByVal sender As Object, ByVal Item As UndoableEventArgs)

    Public Sub NewState(ByVal state As UndoableItem)
        If Not IsNothing(topLevel) Then
            topLevel.NextState = state
        End If

        state.PreviousState = topLevel
        current = state
        topLevel = state
        States.Add(state)
    End Sub
    Public ReadOnly Property CanUndo As Boolean
        Get
            Dim dataUndo As UndoableItem = Nothing
            If Not IsNothing(current) Then
                dataUndo = current.PreviousState
            End If
            If Not IsNothing(dataUndo) Then
                Return True
            Else
                Return False
            End If
        End Get
    End Property
    Public ReadOnly Property CanRedo As Boolean
        Get
            Dim dataRedo As UndoableItem = Nothing
            If Not IsNothing(current) Then
                dataRedo = current.NextState
            End If
            If Not IsNothing(dataRedo) Then
                Return True
            Else
                Return False
            End If
        End Get
    End Property
    Public Sub Undo()
        Dim dataUndo As UndoableItem = Nothing
        If Not IsNothing(current) Then
            dataUndo = current.PreviousState
        End If
        If Not IsNothing(dataUndo) Then
            current = dataUndo
            RaiseEvent Load(Me, New UndoableEventArgs(dataUndo))
        End If
    End Sub
    Public Sub Redo()
        Dim dataRedo As UndoableItem = Nothing
        If Not IsNothing(current) Then
            dataRedo = current.NextState
        End If
        If Not IsNothing(dataRedo) Then
            current = dataRedo
            RaiseEvent Load(Me, New UndoableEventArgs(dataRedo))
        End If
    End Sub
    Public Sub LoadState(ByVal item As UndoableItem)
        If Not IsNothing(item) Then
            current = item
            RaiseEvent Load(Me, New UndoableEventArgs(item))
        End If
    End Sub
    Public ReadOnly Property Items As UndoableItem()
        Get
            Dim res As New List(Of UndoableItem)
            Dim item As UndoableItem = current
            Do
                If IsNothing(item) Then Exit Do
                res.Add(item)
                item = item.NextState
            Loop
            If Not IsNothing(current) Then
                item = current.PreviousState
                Do
                    If IsNothing(item) Then Exit Do
                    If res.Count = 0 Then
                        res.Add(item)
                    Else
                        res.Insert(0, item)
                    End If
                    item = item.PreviousState
                Loop
            End If
            Return res.ToArray
        End Get
    End Property
    Public Sub Clear()
        current = Nothing
        topLevel = Nothing
        States.Clear()
    End Sub
End Class

Public MustInherit Class UndoableItem
    Friend Property PreviousState As UndoableItem
    Friend Property NextState As UndoableItem
    Public Property Name As String
    Public Property Data As Object

End Class

Public Class UndoableEventArgs
    Inherits EventArgs
    Public ReadOnly Property Item As UndoableItem
    Public Sub New(ByVal item As UndoableItem)
        Me.Item = item
    End Sub

End Class