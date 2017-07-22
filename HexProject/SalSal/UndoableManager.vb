Public Class UndoableManager
    Implements IDisposable

    Friend cur As UndoableItem
    Friend top As UndoableItem
    Friend lim As Integer

    Public Event Load(ByVal sender As Object, ByVal Item As UndoableEventArgs)
    Public Event ItemAdded(ByVal sender As Object, ByVal Item As UndoableEventArgs)
    Public Sub New()

    End Sub
    Public Sub New(ByVal maximum As Integer)
        If maximum < 0 Then maximum = 0
        Me.lim = maximum
    End Sub

    Public ReadOnly Property Current As UndoableItem
        Get
            Return cur
        End Get
    End Property
    Public ReadOnly Property Last As UndoableItem
        Get
            Return top
        End Get
    End Property
    Public Property Maximum As Integer
        Get
            Return lim
        End Get
        Set(value As Integer)
            If value < 1 Then value = 0
            lim = value
            SetLimit()
        End Set
    End Property
    Public Sub AppendState(ByVal state As UndoableItem)
        Dim prevItem As UndoableItem = Nothing

        If Not cur Is top Then
            Dim cloneItem As UndoableItem = cur.Clone
            prevItem = cloneItem
            If Not IsNothing(top) Then
                top.NextState = cloneItem
            End If
            cloneItem.PreviousState = top
            cloneItem.NextState = state
            state.PreviousState = cloneItem
            cur = state
            top = state
        Else
            If Not IsNothing(top) Then
                top.NextState = state
            End If
            state.PreviousState = top
            cur = state
            top = state
        End If

        SetLimit()

        If Not IsNothing(prevItem) Then RaiseEvent ItemAdded(Me, New UndoableEventArgs(prevItem))
        RaiseEvent ItemAdded(Me, New UndoableEventArgs(state))
    End Sub
    Public Sub AppendState(ByVal Name As String, ByVal Data As Object)
        Dim item As New UndoableItem(Name, Data)
        AppendState(item)
    End Sub
    Public Sub AppendState(ByVal Data As Object)
        Dim item As New UndoableItem("", Data)
        AppendState(item)
    End Sub
    Friend Sub SetLimit()
        If Not lim = 0 Then
            Dim item As UndoableItem = top
            Dim c As Integer = 0
            If Not IsNothing(item) Then
                Do
                    If IsNothing(item.PreviousState) Then Exit Do
                    c += 1
                    If c >= lim Then
                        item.PreviousState = Nothing
                        Exit Do
                    End If
                    item = item.PreviousState
                Loop
            End If
        End If
    End Sub
    Public ReadOnly Property CanUndo As Boolean
        Get
            Dim dataUndo As UndoableItem = Nothing
            If Not IsNothing(cur) Then
                dataUndo = cur.PreviousState
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
            If Not IsNothing(cur) Then
                dataRedo = cur.NextState
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
        If Not IsNothing(cur) Then
            dataUndo = cur.PreviousState
        End If
        If Not IsNothing(dataUndo) Then
            cur = dataUndo
            RaiseEvent Load(Me, New UndoableEventArgs(dataUndo))
        End If
    End Sub
    Public Sub Redo()
        Dim dataRedo As UndoableItem = Nothing
        If Not IsNothing(cur) Then
            dataRedo = cur.NextState
        End If
        If Not IsNothing(dataRedo) Then
            cur = dataRedo
            RaiseEvent Load(Me, New UndoableEventArgs(dataRedo))
        End If
    End Sub
    Public Sub LoadState(ByVal item As UndoableItem)
        If Not IsNothing(item) Then
            cur = item
            RaiseEvent Load(Me, New UndoableEventArgs(item))
        End If
    End Sub
    Public ReadOnly Property Items As UndoableItem()
        Get
            Dim res As New List(Of UndoableItem)
            Dim item As UndoableItem = top
            If Not IsNothing(item) Then
                Do
                    If IsNothing(item.PreviousState) Then Exit Do
                    item = item.PreviousState
                Loop
                If Not IsNothing(item) Then
                    Do
                        If IsNothing(item) Then Exit Do
                        res.Add(item)
                        item = item.NextState
                    Loop
                End If
            End If
            Return res.ToArray
        End Get
    End Property
    Public Sub Clear()
        cur = Nothing
        top = Nothing
    End Sub

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
        GC.SuppressFinalize(Me)
    End Sub
#End Region
End Class

Public Class UndoableItem
    Implements ICloneable

    Friend PreviousState As UndoableItem
    Friend NextState As UndoableItem
    Public Property Name As String
    Public Property Data As Object

    Public Sub New()

    End Sub
    Public Sub New(ByVal Name As String, ByVal Data As Object)
        Me.Name = Name
        Me.Data = Data
    End Sub
    Public Overridable Function Clone() As Object Implements ICloneable.Clone
        Return Me.MemberwiseClone()
    End Function
    Public Overrides Function ToString() As String
        Return Name.ToString
    End Function
End Class

Public Class UndoableEventArgs
    Inherits EventArgs
    Public ReadOnly Property Item As UndoableItem
    Public Sub New(ByVal item As UndoableItem)
        Me.Item = item
    End Sub

End Class