Public Class SelectionManager
    Friend itms As New List(Of SelectionItem)
    Friend Property tb As HexBox
    Friend trans As ITransformer
    Friend Sub New(tb As HexBox, ByVal trans As ITransformer)
        Me.tb = tb
        Me.trans = trans
    End Sub

#Region "Old"
    Public ReadOnly Property Items As List(Of SelectionItem)
        Get
            If itms.Count = 0 Then
                itms.Add(New SelectionItem(Me, 0, 0, False))
            End If
            Return itms
        End Get
    End Property
    Public ReadOnly Property Curent As SelectionItem
        Get
            Return Items.Last
        End Get
    End Property
    Friend Function AddSelection() As SelectionItem
        Return AddSelection(0, 0, False)
    End Function
    Public Function AddSelection(ByVal position As Long, ByVal Length As Long) As SelectionItem
        Backup()
        Dim sItem As SelectionItem = AddSelection(position, position + Length, False)
        tb.InvalidateSEL(Me, trans, sItem)
        Return sItem
    End Function
    Public Function AddSelectionByIndex(ByVal index As Long, ByVal Length As Long) As SelectionItem
        Backup()
        Dim thisBox As BoxItem = tb.SM.Box(tb.FocussedBoxIndex)
        Dim lpd As Integer = thisBox.trans.LengthPerData
        For Each i In tb.SM.Box
            If i.trans.LengthPerData < lpd Then
                lpd = i.trans.LengthPerData
                thisBox = i
            End If
        Next

        Dim trans As ITransformer = thisBox.trans
        Dim cpr As Integer = tb.GetCharsPerRow(thisBox)

        Dim line As Long = Math.Floor(index / tb.SM.col_count) * cpr
        Dim col As Integer = Math.Floor((index Mod tb.SM.col_count) / trans.LengthPerData) * (trans.CharsPerData + trans.Sparator)
        Dim position As Long = line + col

        line = Math.Floor(Length / tb.SM.col_count) * cpr
        col = Math.Ceiling((Length Mod tb.SM.col_count) / trans.LengthPerData) * (trans.CharsPerData + trans.Sparator)
        If col > cpr Then col = cpr

        Length = line + col

        Dim sItem As SelectionItem = AddSelection(position, position + Length, False)
        tb.InvalidateSEL(Me, trans, sItem)
        Return sItem
    End Function
    Public Function AddSelection(Anchor As Long, Caret As Long, LF As Boolean) As SelectionItem
        Dim item As New SelectionItem(Me, Anchor, Caret, LF)
        itms.Add(item)
        Return item
    End Function
    Public Function IsSelected(ByVal position As Long) As Boolean
        Return IsSelected(position, Nothing)
    End Function
    Friend Function IsSelected(ByVal index As Long, ByRef outItem As SelectionItem) As Boolean
        If SelectionLength = 0 Then Return False
        For Each i In Items
            Dim smin As Long = i.anc
            Dim smax As Long = i.car
            If smin > smax Then
                smin = i.car
                smax = i.anc
            End If
            If smin <= index And index < smax Then
                outItem = i
                Return True
            End If
        Next
        Return False
    End Function
    Friend Function IsSelected(ByVal Item As SelectionItem, ByVal index As Long) As Boolean
        If Math.Abs(Item.car - Item.anc) = 0 Then Return False
        Dim smin As Long = Item.anc
        Dim smax As Long = Item.car
        If smin > smax Then
            smin = Item.car
            smax = Item.anc
        End If
        If smin <= index And index < smax Then
            Return True
        End If
        Return False
    End Function
    Public Function Clone() As SelectionManager
        Dim sm As New SelectionManager(tb, trans)

        For Each i In Me.Items
            sm.itms.Add(i.Clone)
        Next

        Return sm
    End Function
    Friend Function TransformSelection(ByVal source As ITransformer, ByVal target As ITransformer) As SelectionManager
        Dim sm As New SelectionManager(tb, source)

        Dim perData As Integer = source.CharsPerData + source.Sparator
        Dim colFloor As Integer = Math.Floor(tb.SM.col_count / source.LengthPerData) * source.LengthPerData
        Dim charsPerRow As Integer = (colFloor / source.LengthPerData * perData)
        Dim charsWhitoutRedZonePerRow As Integer = (colFloor / source.LengthPerData * perData)
        Dim redZoneLength As Integer = (tb.SM.col_count - colFloor) * 2
        ' If (tb.SM.col_count - colFloor) > 0 Then redZoneLength += source.Sparator
        charsPerRow += redZoneLength

        Dim charsPerRowSource As Integer = charsPerRow
        Dim redZoneLengthSource As Integer = redZoneLength

        perData = target.CharsPerData + target.Sparator
        colFloor = Math.Floor(tb.SM.col_count / target.LengthPerData) * target.LengthPerData
        charsPerRow = (colFloor / target.LengthPerData * perData)
        charsWhitoutRedZonePerRow = (colFloor / target.LengthPerData * perData)
        redZoneLength = (tb.SM.col_count - colFloor) * 2
        ' If (tb.SM.col_count - colFloor) > 0 Then redZoneLength += target.Sparator
        charsPerRow += redZoneLength

        Dim charsPerRowTarget As Integer = charsPerRow
        Dim redZoneLengthTarget As Integer = redZoneLength

        For Each itm As SelectionItem In Items
            Dim Anchor As Long = itm.anc
            Dim Caret As Long = itm.car
            Dim LF As Boolean = itm.LF

            Dim data As Long() = {Anchor, Caret}

            For i As Integer = 0 To data.Length - 1
                Dim lineSource As Long = Math.Floor(data(i) / charsPerRowSource)
                Dim linePosSource As Long = lineSource * tb.SM.col_count
                Dim linePosTarget As Long = lineSource * charsPerRowTarget
                Dim linePosSource2 As Long = lineSource * charsPerRowSource
                Dim isAdd As Boolean = False
                Dim md As Long = data(i) Mod charsPerRowSource
                Dim md2 As Long = md Mod (source.CharsPerData + source.Sparator)
                If Not md2 = 0 And i = 1 Then
                    isAdd = True
                End If

                Dim indexLineSource As Long = lineSource * tb.SM.col_count

                Dim indexCharSource As Long = data(i) - lineSource * charsPerRowSource
                Dim indexColSource As Long = Math.Floor(indexCharSource / (source.CharsPerData + source.Sparator))
                Dim indexColTarget As Long = Math.Floor(indexColSource / target.LengthPerData * source.LengthPerData) * (target.CharsPerData + target.Sparator)

                Dim res As Long = linePosTarget + indexColTarget

                Dim targetIsInRedZone As Boolean = False
                Dim hLength As Integer = 0
                If Not redZoneLengthTarget = 0 Then
                    targetIsInRedZone = indexColTarget >= redZoneLengthTarget
                    If targetIsInRedZone Then
                        hLength = redZoneLengthTarget
                    Else
                        hLength = target.CharsPerData + target.Sparator
                        If target.LengthPerData < source.LengthPerData Then
                            hLength = hLength * source.LengthPerData / target.LengthPerData
                        End If
                    End If
                Else
                    hLength = target.CharsPerData + target.Sparator
                    If target.LengthPerData < source.LengthPerData Then
                        Dim isSourceContainsRedzone As Boolean = ((tb.SM.col_count Mod source.LengthPerData) = 0)
                        Dim sourceIsInRedZone As Boolean = indexColSource = Math.Floor(tb.SM.col_count / source.LengthPerData)
                        If sourceIsInRedZone Then
                            hLength = hLength * source.LengthPerData / target.LengthPerData
                            If isAdd Then
                                If (res Mod charsPerRowTarget) + hLength > charsPerRowTarget Then
                                    hLength = charsPerRowTarget - (res Mod charsPerRowTarget)
                                End If
                            End If
                        Else
                            hLength = hLength * source.LengthPerData / target.LengthPerData
                        End If
                    End If
                End If

                If hLength > charsPerRow Then hLength = charsPerRow
                If i = 0 And Anchor > Caret Then
                    If isAdd Then res += hLength
                ElseIf i = 1 And Caret > Anchor Then
                    If isAdd Then res += hLength
                End If

                data(i) = res
            Next

            Dim sItem As New SelectionItem(Me, data(0), data(1), LF)
            sm.Items.Add(sItem)
        Next
        Return sm
    End Function
    Friend Function TransformItem(ByVal Item As SelectionItem, ByVal source As ITransformer, ByVal target As ITransformer) As SelectionItem
        Dim perData As Integer = source.CharsPerData + source.Sparator
        Dim colFloor As Integer = Math.Floor(tb.SM.col_count / source.LengthPerData) * source.LengthPerData
        Dim charsPerRow As Integer = (colFloor / source.LengthPerData * perData)
        Dim charsWhitoutRedZonePerRow As Integer = (colFloor / source.LengthPerData * perData)
        Dim redZoneLength As Integer = (tb.SM.col_count - colFloor) * 2
        'If (tb.SM.col_count - colFloor) > 0 Then redZoneLength += source.Sparator
        charsPerRow += redZoneLength

        Dim charsPerRowSource As Integer = charsPerRow
        Dim redZoneLengthSource As Integer = redZoneLength

        perData = target.CharsPerData + target.Sparator
        colFloor = Math.Floor(tb.SM.col_count / target.LengthPerData) * target.LengthPerData
        charsPerRow = (colFloor / target.LengthPerData * perData)
        charsWhitoutRedZonePerRow = (colFloor / target.LengthPerData * perData)
        redZoneLength = (tb.SM.col_count - colFloor) * 2
        'If (tb.SM.col_count - colFloor) > 0 Then redZoneLength += target.Sparator
        charsPerRow += redZoneLength

        Dim charsPerRowTarget As Integer = charsPerRow
        Dim redZoneLengthTarget As Integer = redZoneLength

        Dim Anchor As Long = Item.anc
        Dim Caret As Long = Item.car
        Dim LF As Boolean = Item.LF

        Dim data As Long() = {Anchor, Caret}

        For i As Integer = 0 To data.Length - 1
            Dim lineSource As Long = Math.Floor(data(i) / charsPerRowSource)
            Dim linePosSource As Long = lineSource * tb.SM.col_count
            Dim linePosTarget As Long = lineSource * charsPerRowTarget
            Dim linePosSource2 As Long = lineSource * charsPerRowSource
            Dim isAdd As Boolean = False
            Dim md As Long = data(i) Mod charsPerRowSource
            Dim md2 As Long = md Mod (source.CharsPerData + source.Sparator)
            If Not md2 = 0 And i = 1 Then
                isAdd = True
            End If

            Dim indexLineSource As Long = lineSource * tb.SM.col_count

            Dim indexCharSource As Long = data(i) - lineSource * charsPerRowSource
            Dim indexColSource As Long = Math.Floor(indexCharSource / (source.CharsPerData + source.Sparator))
            Dim indexColTarget As Long = Math.Floor(indexColSource / target.LengthPerData * source.LengthPerData) * (target.CharsPerData + target.Sparator)

            Dim res As Long = linePosTarget + indexColTarget

            Dim targetIsInRedZone As Boolean = False
            Dim hLength As Integer = 0
            If Not redZoneLengthTarget = 0 Then
                targetIsInRedZone = indexColTarget >= redZoneLengthTarget
                If targetIsInRedZone Then
                    hLength = redZoneLengthTarget
                Else
                    hLength = target.CharsPerData + target.Sparator
                    If target.LengthPerData < source.LengthPerData Then
                        hLength = hLength * source.LengthPerData / target.LengthPerData
                    End If
                End If
            Else
                hLength = target.CharsPerData + target.Sparator
                If target.LengthPerData < source.LengthPerData Then
                    Dim isSourceContainsRedzone As Boolean = ((tb.SM.col_count Mod source.LengthPerData) = 0)
                    Dim sourceIsInRedZone As Boolean = indexColSource = Math.Floor(tb.SM.col_count / source.LengthPerData)
                    If sourceIsInRedZone Then
                        hLength = hLength * source.LengthPerData / target.LengthPerData
                        If isAdd Then
                            If (res Mod charsPerRowTarget) + hLength > charsPerRowTarget Then
                                hLength = charsPerRowTarget - (res Mod charsPerRowTarget)
                            End If
                        End If
                    Else
                        hLength = hLength * source.LengthPerData / target.LengthPerData
                    End If
                End If
            End If

            If hLength > charsPerRow Then hLength = charsPerRow
            If i = 0 And Anchor > Caret Then
                If isAdd Then res += hLength
            ElseIf i = 1 And Caret > Anchor Then
                If isAdd Then res += hLength
            End If

            data(i) = res
        Next

        Return New SelectionItem(Me, data(0), data(1), Item.LF)
    End Function
    Friend Sub SnapSelection(ByVal Item As SelectionItem)
        Dim transformer As ITransformer = tb.SM.Box(tb.FocussedBoxIndex).trans
        SnapSelection(Item, transformer, SnapEffect.All)
    End Sub
    Friend Sub SnapSelection(ByVal Item As SelectionItem, ByVal effect As SnapEffect)
        Dim transformer As ITransformer = tb.SM.Box(tb.FocussedBoxIndex).trans
        SnapSelection(Item, transformer, effect)
    End Sub
    Friend Sub SnapSelection(ByVal Item As SelectionItem, ByVal transformer As ITransformer, ByVal effect As SnapEffect)
        Dim transItem As SelectionItem = TransformItem(transformer, Item)
        If effect.HasFlag(SnapEffect.Anchor) Then
            Item.anc = transItem.anc
        End If
        If effect.HasFlag(SnapEffect.Caret) Then
            Item.car = transItem.car
            Item.LF = transItem.LF
        End If
    End Sub
    Friend Function IsAnchor(ByVal position As Long) As Boolean
        Return position = Curent.anc
    End Function
    Friend Function IsCaret(ByVal position As Long) As Boolean
        Dim sItem As SelectionItem = Curent

        If sItem.LF Then
            Return position = sItem.car - 1
        Else
            Return position = sItem.car
        End If
    End Function
    Friend Function IsOver(ByVal position As Long, ByVal TI As AdvancedTransformInfo) As Boolean
        Dim Item As SelectionItem = Me.Items.Last
        Dim Caret As Long = Item.car
        If Item.LF Then Caret -= 1

        Dim linePos As Long = Math.Floor(position / TI.CharsPerRow)
        Dim line As Long = Math.Floor(Caret / TI.CharsPerRow)

        If line = linePos Then
            Dim pos As Integer = position Mod TI.CharsPerRow
            Dim col As Integer = Caret Mod TI.CharsPerRow
            Dim col_mod As Integer = Math.Floor(col / TI.PerData) * TI.PerData
            Dim col_max As Integer = col_mod + TI.PerData
            If col_max > TI.CharsPerRow Then col_max = TI.CharsPerRow

            If col_mod <= pos And pos < col_max Then
                Return True
            End If
        End If
        Return False
    End Function
    Friend Function IsOver(ByVal position As Long, ByVal target As ITransformer) As Boolean
        Dim perData As Integer = trans.CharsPerData + trans.Sparator
        Dim colFloor As Integer = Math.Floor(tb.SM.col_count / trans.LengthPerData) * trans.LengthPerData
        Dim charsPerRow As Integer = (colFloor / trans.LengthPerData * perData)
        Dim charsWhitoutRedZonePerRow As Integer = (colFloor / trans.LengthPerData * perData)
        Dim redZoneLength As Integer = (tb.SM.col_count - colFloor) * 2
        charsPerRow += redZoneLength

        Dim charsPerRowSource As Integer = charsPerRow
        Dim redZoneLengthSource As Integer = redZoneLength

        perData = target.CharsPerData + target.Sparator
        colFloor = Math.Floor(tb.SM.col_count / target.LengthPerData) * target.LengthPerData
        charsPerRow = (colFloor / target.LengthPerData * perData)
        charsWhitoutRedZonePerRow = (colFloor / target.LengthPerData * perData)
        redZoneLength = (tb.SM.col_count - colFloor) * 2
        'If (tb.SM.col_count - colFloor) > 0 Then redZoneLength += target.Sparator
        charsPerRow += redZoneLength

        Dim charsPerRowTarget As Integer = charsPerRow
        Dim redZoneLengthTarget As Integer = redZoneLength

        Dim Item As SelectionItem = Me.Items.Last
        Dim Caret As Long = Item.car
        If Item.LF Then
            Caret -= 1
        End If
        Dim lineSource As Long = Math.Floor(Caret / charsPerRowSource)
        Dim linePosSource As Long = lineSource * tb.SM.col_count
        Dim linePosTarget As Long = lineSource * charsPerRowTarget

        Dim indexLineSource As Long = lineSource * tb.SM.col_count

        Dim indexCharSource As Long = Caret - lineSource * charsPerRowSource
        Dim indexColSource As Long = Math.Floor(indexCharSource / (trans.CharsPerData + trans.Sparator))
        Dim indexColTarget As Long = Math.Floor(indexColSource / target.LengthPerData * trans.LengthPerData) * (target.CharsPerData + target.Sparator)

        Dim Caret2 As Long = linePosTarget + indexColTarget

        Dim targetIsInRedZone As Boolean = False
        Dim hLength As Integer = 0
        If Not redZoneLengthTarget = 0 Then
            targetIsInRedZone = indexColTarget >= redZoneLengthTarget
            If targetIsInRedZone Then
                hLength = redZoneLengthTarget + target.Sparator
            Else
                hLength = target.CharsPerData + target.Sparator
                If target.LengthPerData < trans.LengthPerData Then
                    hLength = hLength * trans.LengthPerData / target.LengthPerData
                End If
            End If
            hLength -= target.Sparator
        Else
            hLength = target.CharsPerData + target.Sparator
            If target.LengthPerData < trans.LengthPerData Then
                Dim isSourceContainsRedzone As Boolean = ((tb.SM.col_count Mod trans.LengthPerData) = 0)
                Dim sourceIsInRedZone As Boolean = indexColSource = Math.Floor(tb.SM.col_count / trans.LengthPerData)
                If sourceIsInRedZone Then
                    hLength = hLength * trans.LengthPerData / target.LengthPerData
                    If (Caret2 Mod charsPerRowTarget) + hLength > charsPerRowTarget Then
                        hLength = charsPerRowTarget - (Caret2 Mod charsPerRowTarget)
                    End If
                Else
                    hLength = hLength * trans.LengthPerData / target.LengthPerData
                End If
            End If
            hLength -= target.Sparator
        End If

        If hLength > charsPerRow Then hLength = charsPerRow
        Return Caret2 <= position And position < (Caret2 + hLength)
    End Function
    Public ReadOnly Property SelectionLength() As Long
        Get
            Dim totalSelection As Long = 0
            For Each i In Items
                totalSelection += Math.Abs(i.car - i.anc)
            Next
            Return totalSelection
        End Get
    End Property
    Public Sub Clear()
        Backup()
        itms.Clear()
        Invalidate()
    End Sub
    Friend bak As SelectionManager
    Friend Sub Backup()
        If tb.SL Is Me Then
            bak = Me.Clone
        End If
    End Sub
    Friend Sub Invalidate()
        If tb.SL Is Me And Not IsNothing(bak) Then
            tb.InvalidateSelection(bak)
        End If
    End Sub
#End Region

#Region "New"
    Public Function CreateTransform(ByVal destTrans As ITransformer) As SelectionManager
        Dim SM_t As New SelectionManager(tb, destTrans)

        For Each i In Items
            Dim item_t As SelectionItem = TransformItem(destTrans, i)
            item_t.SM = SM_t
            SM_t.itms.Add(item_t)
        Next

        Return SM_t
    End Function

    Public Function TransformItem(ByVal destTrans As ITransformer, ByVal Item As SelectionItem) As SelectionItem
        Dim afterLength As Long = tb.bb.GetLength + tb.shift_val_pre
        Dim tISource As AdvancedTransformInfo = Transformers.GetAdvancedTransformInfo(trans, tb.SM.col_count, afterLength)
        Dim tIDest As AdvancedTransformInfo = Transformers.GetAdvancedTransformInfo(destTrans, tb.SM.col_count, afterLength)

        Dim v As Long() = {Item.anc, Item.car}
        Dim v_t(v.Length - 1) As Long

        Dim c As Long = Me.SelectionLength
        Dim LF As Boolean = Item.LF

        For i As Integer = 0 To 1
            Dim val As Long = v(i)
            Dim line As Long = Math.Floor(val / tISource.CharsPerRow)
            Dim col As Integer = val Mod tISource.CharsPerRow
            If Not col Mod tISource.PerData = 0 And Not c = 0 Then
                If i = 0 Then
                    If Item.anc < Item.car Then
                        col = Math.Floor(col / tISource.PerData) * tISource.PerData
                    Else
                        col = Math.Ceiling(col / tISource.PerData) * tISource.PerData
                    End If
                ElseIf i = 1 Then
                    If Item.car > Item.anc Then
                        col = Math.Ceiling(col / tISource.PerData) * tISource.PerData
                    Else
                        col = Math.Floor(col / tISource.PerData) * tISource.PerData
                    End If
                End If
            End If
            Dim col_b As Integer = Math.Floor(col / tISource.PerData)
            Dim col_t As Integer = Math.Floor(col_b * tISource.Trans.LengthPerData / tIDest.Trans.LengthPerData) * tIDest.PerData
            If col_t > tIDest.CharsPerRow Then
                col_t = tIDest.CharsPerRow
                LF = True
            End If
            Dim line_t As Long = line * tIDest.CharsPerRow
            Dim val_t As Long = line_t + col_t

            If val_t > tIDest.MaxAllCharsLength Then
                val_t = tIDest.MaxAllCharsLength
            End If

            v_t(i) = val_t
        Next

        Dim Item_t As New SelectionItem
        Item_t.anc = v_t(0)
        Item_t.car = v_t(1)
        Item_t.LF = LF

        Return Item_t
    End Function
    Friend Function ArrangeSelection() As SelectionManager
        Dim list2 As List(Of SelectionItem) = Nothing
        Dim TI As AdvancedTransformInfo = Transformers.GetAdvancedTransformInfo(Me.trans, tb.SM.col_count, tb.bb.GetLength + tb.shift_val_pre)
        Dim mySel As SelectionManager = Me.Clone

        Do
            Dim list1 As List(Of SelectionItem) = mySel.Items
            list2 = New List(Of SelectionItem)
            Dim c As Integer = 0
            For Each i In list1
                Dim col As Integer = i.SelectionStart Mod TI.CharsPerRow
                Dim col_t As Integer = Math.Floor(col / TI.PerData) * TI.PerData
                Dim col2 As Integer = (i.SelectionStart + i.SelectionLength) Mod TI.CharsPerRow
                Dim col2_t As Integer = Math.Floor(col2 / TI.PerData) * TI.PerData

                Dim a1 As Long = Math.Floor(i.SelectionStart / TI.CharsPerRow) * TI.CharsPerRow + col_t
                Dim a2 As Long = Math.Floor((i.SelectionStart + i.SelectionLength) / TI.CharsPerRow) * TI.CharsPerRow + col2_t
                Dim b1 As Long = 0
                Dim b2 As Long = 0
                Dim isIntersect As Boolean = False
                Dim intersectWith As SelectionItem = Nothing

                For Each i2 In list2
                    Dim xcol As Integer = i2.SelectionStart Mod TI.CharsPerRow
                    Dim xcol_t As Integer = Math.Floor(xcol / TI.PerData) * TI.PerData
                    Dim xcol2 As Integer = (i2.SelectionStart + i2.SelectionLength) Mod TI.CharsPerRow
                    Dim xcol2_t As Integer = Math.Floor(xcol2 / TI.PerData) * TI.PerData

                    b1 = Math.Floor(i2.SelectionStart / TI.CharsPerRow) * TI.CharsPerRow + xcol_t
                    b2 = Math.Floor((i2.SelectionStart + i2.SelectionLength) / TI.CharsPerRow) * TI.CharsPerRow + xcol2_t

                    If Helper.IntersectsWith(a1, a2, b1, b2) Then
                        isIntersect = True
                        intersectWith = i2
                        Exit For
                    End If
                Next

                If isIntersect Then
                    Dim str As Long = Math.Min(a1, b1)
                    Dim snd As Long = Math.Min(a2, b2)
                    intersectWith.anc = str
                    intersectWith.car = snd
                    c += 1
                Else
                    list2.Add(i)
                End If
            Next

            If c = 0 Then Exit Do
            list1 = list2
        Loop

        Dim SL2 As New SelectionManager(Me.tb, Me.trans)
        SL2.itms.AddRange(list2)

        Return SL2
    End Function

#End Region
End Class
Public Class SelectionItem
    Friend anc As Long
    Friend car As Long
    Friend LF As Boolean
    Friend SM As SelectionManager
    Public Property Anchor As Long
        Get
            Return anc
        End Get
        Set(value As Long)
            SM.Backup()
            anc = value
            SM.Invalidate()
        End Set
    End Property
    Public Property Caret As Long
        Get
            Return car
        End Get
        Set(value As Long)
            SM.Backup()
            car = value
            SM.Invalidate()
        End Set
    End Property
    Public Property SelectionStart As Long
        Get
            If anc < car Then
                Return anc
            Else
                Return car
            End If
        End Get
        Set(value As Long)
            SM.Backup()

            Dim len As Long = Math.Abs(car - anc)
            anc = value
            car = anc + len

            SM.Invalidate()
        End Set
    End Property

    Public Property SelectionLength() As Long
        Get
            Dim len As Long = Math.Abs(car - anc)
            Return len
        End Get
        Set(value As Long)
            SM.Backup()

            Dim len As Long = Math.Abs(car - anc)
            If anc > car Then
                anc = car
            End If
            car = anc + len

            SM.Invalidate()
        End Set
    End Property

    Friend Sub New()

    End Sub
    Friend Sub New(SM As SelectionManager, Anchor As Long, Caret As Long, LF As Boolean)
        Me.SM = SM
        Me.anc = Anchor
        Me.car = Caret
        Me.LF = LF
    End Sub
    Friend Function Clone() As SelectionItem
        Return New SelectionItem(SM, anc, car, LF)
    End Function
End Class
Friend Enum SnapEffect As Integer
    Anchor = 1
    Caret = 2
    All = 3
End Enum