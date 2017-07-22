Imports System.ComponentModel
Imports System.Drawing
Imports System.Drawing.Imaging
Imports System.IO
Imports System.Runtime.InteropServices
Imports System.Windows.Forms

Public Class HexBox
    Friend WithEvents bb As SalSal.ByteBuilder
    Friend SM As New StyleSet(Me)
    Friend QR As QuickReader
    Friend SL As New SelectionManager(Me, SM.Box(0).trans)

    Friend ShowedStart As Long
    Friend ShowedRowCount As Integer
    Friend ShowedLength As Integer
    Friend FocussedBoxIndex As Integer = 0
    Friend extraW As Integer = 0 'extra width

    'scrollbars
    Friend vscroll_val As Long
    Friend hscroll_val As Long
    Friend vscroll_max_scroll As Integer
    Friend vscroll_max_value As Long
    Friend vscroll_enhanced As Boolean
    Friend limit_vscroll As Integer = 2000000000

    Friend ReadOnly Property shift_val_pre As Long
        Get
            If slider_push Then
                Return shift_val
            Else
                Return -shift_val
            End If
        End Get
    End Property
    Friend shift_val As Long = 0
    Friend slider_show As Boolean = False
    Friend slider_val As Integer = 0
    Friend slider_hot As Boolean
    Friend slider_down As Boolean
    Friend sliderDownX As Integer
    Friend slider_size As New Size(20, 10)
    Friend slider_push As Boolean = True
    Friend last_copy_overflow As Boolean

    Friend MyRect As Rectangle
    Friend DrawInvalidateArea As Boolean '= True

    Friend WF As New List(Of WrongFormatted)
    Friend menuItems As New List(Of MenuItem)

    Friend nodrawable_chars As Char() = {Chr(0), Chr(1), Chr(2), Chr(3), Chr(4), Chr(5), Chr(6), Chr(7), Chr(8), Chr(9), Chr(&HA), Chr(&HB), Chr(&HC), Chr(&HD), Chr(&HE), Chr(&HF),
                                        Chr(&H10), Chr(&H11), Chr(&H12), Chr(&H13), Chr(&H14), Chr(&H15), Chr(&H16), Chr(&H17), Chr(&H18), Chr(&H19), Chr(&H1A), Chr(&H1B), Chr(&H1C), Chr(&H1D), Chr(&H1E), Chr(&H1F),
                                        Chr(&H20), Chr(&H7F), Chr(&H81), Chr(&H8D), Chr(&H8F), Chr(&H90), Chr(&H98), Chr(&H9D), Chr(&HAD), ChrW(65279)}

    Friend ub As New UnAccessableByte(Me)
    Friend ue As New UnEditableByte(Me)
    Friend ubSymbol As Char = "?" 'UnEditable symbol

    Friend ro As Boolean = False  'readOnly
    Friend maxcp As Integer = UShort.MaxValue 'max copy length

    Public Event SelectionChanged(ByVal sender As Object, ByVal e As HexBoxSelectionEventArgs)
    Public Event GDI32Paint(ByVal sender As Object, ByVal e As GDI32PaintEventArgs)
    Public Event ContentPaint(ByVal sender As Object, ByVal e As ContentPaintEventArgs)

    Public Sub New()

        ' This call is required by the designer.
        InitializeComponent()

        ' Add any initialization after the InitializeComponent() call.

        Dim fs As New MemoryStream
        bb = New SalSal.ByteBuilder(fs)
        QR = New QuickReader(bb)

        SetStyle(ControlStyles.UserPaint, True)
        'SetStyle(ControlStyles.ContainerControl, True)
        SetStyle(ControlStyles.Selectable, True)
        'SetStyle(USER32.WindowStyles.WS_VSCROLL, True)
        'SetStyle(USER32.WindowStyles.WS_HSCROLL, True)
        TimerIndicator.Start()
        ' AddWrongFormatted(SM.Box(0), 0, "this is bad")
        Dim menu As String() = {"&Expand....", "|", "&Undo", "&Redo", "|", "&Goto....", "|", "Cu&t", "&Copy", "Advanced Copy", "&Paste", "&Delete", "|", "Select &All"}
        AddMenu(menuItems, menu)

        menu = {"ANSI", "Array of Bytes", "Unicode", "|", "Current Box Content", "Current Box Content + Offset", "|", "All Box Content", "All Box Content + Offset", "|", "Full Copy"}
        AddMenu(GetMenu("Advanced Copy").Items, menu)

    End Sub

#Region "WndProc"
    Friend Const WM_SETFOCUS As Integer = &H7
    Friend Const WM_CONTEXTMENU As Integer = &H7B
    Friend cancelContextMenu As Boolean = False
    Protected Overrides Sub WndProc(ByRef m As Message)
        Select Case m.Msg
            Case WM_SETFOCUS
                Exit Sub
            Case WM_CONTEXTMENU
                If cancelContextMenu Then
                    cancelContextMenu = False
                    Exit Sub
                End If
                If enContextMenu Then
                    If IsNothing(Me.ContextMenuStrip) Then
                        ShowContextMenu()
                        Me.Select()
                        Me.Focus()
                    End If
                End If
        End Select
        MyBase.WndProc(m)
    End Sub
    Protected Overrides ReadOnly Property CreateParams As CreateParams
        Get
            Dim cp As CreateParams = MyBase.CreateParams
            cp.Style = cp.Style Or USER32.WindowStyles.WS_VSCROLL Or USER32.WindowStyles.WS_HSCROLL
            Return cp
        End Get
    End Property
#End Region

#Region "Drawing"
    Friend renderTime(9) As Long
    Friend renderTimeAvg(9) As Long
    Friend hotData As String = "61"
    Friend hotFind As New List(Of Long)
    Friend toggleHotFind As Boolean
    Friend ftoggleHotFind As Boolean
    Friend dtoggleHotFind As Boolean = True

    Protected Overrides Sub OnPaintBackground(e As PaintEventArgs)

    End Sub
    Protected Overrides Sub OnPaint(e As PaintEventArgs)
        Dim dt As Date = Date.Now
        If e.ClipRectangle.Width < 1 Or e.ClipRectangle.Height < 1 Then
            Exit Sub
        End If
        Dim bmp As New Bitmap(e.ClipRectangle.Width, e.ClipRectangle.Height, PixelFormat.Format32bppPArgb)
        Dim gbmp As Graphics = Graphics.FromImage(bmp)
        Dim hDC As IntPtr = gbmp.GetHdc

        Dim RM As New RenderParam
        RM.hDC = hDC
        RM.translate.X = -e.ClipRectangle.X
        RM.translate.Y = -e.ClipRectangle.Y
        RM.translate.X -= hscroll_val
        RM.size = e.ClipRectangle.Size
        RM.AfterLength = bb.GetLength + shift_val_pre
        RM.WContent = GetContentWidth()
        RM.A = vscroll_val * SM.col_count
        Dim rl As Integer = QR.Update(RM.A, ShowedLength)

        InitializeHotFind()
        RenderAll(RM)

        Dim evt As New GDI32PaintEventArgs(hDC, e.ClipRectangle, RM.translate)
        RaiseEvent GDI32Paint(Me, evt)

        Dim hDCDest As IntPtr = e.Graphics.GetHdc
        GDI32.BitBlt(hDCDest, e.ClipRectangle.X, e.ClipRectangle.Y, bmp.Width, bmp.Height, hDC, 0, 0, GDI32.TernaryRasterOperations.SRCCOPY)

        e.Graphics.ReleaseHdc(hDCDest)
        gbmp.ReleaseHdc(hDC)
        gbmp.Dispose()
        bmp.Dispose()

        If DrawInvalidateArea Then
            Dim br As New SolidBrush(Color.FromArgb(150, Rnd() * 255, Rnd() * 255, Rnd() * 255))
            e.Graphics.FillRectangle(br, 0, 0, MyRect.Width, MyRect.Height)
            br.Dispose()
        End If
        Dim ln As Long = (Date.Now - dt).TotalMilliseconds
        For i As Integer = renderTime.Length - 1 To 1 Step -1
            renderTime(i) = renderTime(i - 1)
        Next
        renderTime(0) = ln

        'e.Graphics.FillRectangle(Brushes.White, 0, 0, 100, 20)
        'e.Graphics.DrawString(ln, SM.Font, Brushes.Red, 0, 0)

    End Sub

    Friend Sub RenderAll(e As RenderParam)
        RenderBackground(e)
        If Not SM.usrpaint_head Then RenderCollumnHeader(e)
        If Not SM.usrpaint_offset Then RenderOffset(e)
        RenderContents(e)
        RenderShiftSlider(e)
        If SM.draw_border Then RenderBorder(e)
        'RenderTimeText(e)
    End Sub
    Friend Sub InitializeHotFind()
        hotFind.Clear()
        If Not ((toggleHotFind Or ftoggleHotFind) And dtoggleHotFind) Then Exit Sub
        Dim hotData As String = ""
        If SL.SelectionLength = 0 Then
            Dim SL As SelectionManager = Me.SL.CreateTransform(SM.Box(0).trans)
            Dim caret As Long = SL.Curent.car
            If SL.Curent.LF Then
                caret -= 1
            End If
            Dim TI As AdvancedTransformInfo = Transformers.GetAdvancedTransformInfo(SL.trans, SM.col_count, bb.GetLength + shift_val_pre)
            Dim p As Long = ToPosition(caret, TI)
            If p < shift_val_pre Then
                hotData = ""
            Else
                p -= shift_val_pre
                Dim lnx As Integer = TI.Trans.LengthPerData
                Dim QR2 As QuickReader = QR
                If Not (QR.Position <= p And (p + lnx - 1) < QR.Position + QR.Length) Then
                    QR2 = New QuickReader(bb, TI.Trans.LengthPerData)
                    QR2.Update(p, lnx)
                End If

                Dim pchr As Long = Math.Floor((caret Mod TI.CharsPerRow) / TI.PerData)
                Dim isPartialX As Boolean = False
                If Not TI.PartialCount = 0 Then
                    If Not pchr < TI.DataCount Then
                        isPartialX = True
                        lnx = TI.PartialCount
                    End If
                End If

                Dim p2 As Long = p - QR2.Position
                If p2 + lnx > QR2.Length Then
                    lnx = QR2.Length - p2
                    isPartialX = True
                End If
                If isPartialX Then
                    Dim subTx As String = ""
                    For i2 As Integer = 0 To lnx - 1
                        subTx &= Helper.Hex2(QR2.Buffer(p2 + i2))
                    Next
                    If subTx.Length > TI.PerData Then
                        subTx = Mid(subTx, 1, TI.PerData)
                    End If
                    hotData = subTx
                Else
                    hotData = TI.Trans.GetString(QR2.Buffer, p2)
                End If
            End If
        End If
        Me.hotData = hotData

        If SL.SelectionLength = 0 And Not hotData = "" Then
            For y As Integer = 0 To ShowedRowCount - 1
                FindSameText(vscroll_val + y)
            Next
        End If
    End Sub
    Friend Sub RenderBackground2(e As RenderParam)
        Dim hBrush As IntPtr = GDI32.CreateSolidBrush(Helper.GetRGB(SM.bkClr))
        Dim hPen As IntPtr = GDI32.GetStockObject(GDI32.StockObjects.NULL_PEN)

        Dim hBrushPrevious As IntPtr = GDI32.SelectObject(e.hDC, hBrush)
        Dim hPenPrevious As IntPtr = GDI32.SelectObject(e.hDC, hPen)

        GDI32.Rectangle(e.hDC, 0, 0, e.size.Width + 1, e.size.Height + 1)

        GDI32.SelectObject(e.hDC, hBrushPrevious)
        GDI32.SelectObject(e.hDC, hPenPrevious)

        GDI32.DeleteObject(hBrush)
    End Sub
    Friend Sub RenderBackground(e As RenderParam)
        Dim rect As New Rectangle(SM.all_padLeft, SM.all_padTop, Math.Max(MyRect.Width, e.WContent + extraW + 20), MyRect.Height + 1)

        If rect.Height <= 0 Then Exit Sub
        rect.X += e.translate.X
        rect.Y += e.translate.Y

        Dim hDC As IntPtr = e.hDC

        If Not e.IsVisible(rect) Then Exit Sub

        Dim hState As Integer = GDI32.SaveDC(hDC)
        Dim hRgn As IntPtr = GDI32.CreateRectRgn(rect.X, rect.Y - 1, rect.Right, rect.Bottom)
        GDI32.SelectClipRgn(hDC, hRgn)

        If Not SM.offset_mode = OffsetMode.Hidden Then
            rect.X += SM.offset_width
        End If

        Dim sourceBox As BoxItem = SM.Box(FocussedBoxIndex)
        Dim w As Integer = 0
        Dim bk As Color = Nothing
        For boxIndex As Integer = 0 To SM.Box.Count
            Dim box As BoxItem = Nothing
            If boxIndex = SM.Box.Count Then
                w = rect.Width
            Else
                box = SM.Box(boxIndex)
                w = box.w
            End If
            bk = SM.bkClr
            If Not IsNothing(box) Then
                If Not IsNothing(box.Style) Then
                    If Not box.Style.BackColor.IsEmpty Then
                        bk = box.Style.BackColor
                    End If
                End If
            End If
            Dim hBrush As IntPtr = GDI32.CreateSolidBrush(Helper.GetRGB(bk))
            Dim hPen As IntPtr = GDI32.GetStockObject(GDI32.StockObjects.NULL_PEN)
            GDI32.SelectObject(e.hDC, hBrush)
            GDI32.SelectObject(e.hDC, hPen)

            GDI32.Rectangle(hDC, rect.X, rect.Y, rect.X + w + 1, rect.Bottom)

            GDI32.DeleteObject(hBrush)

            rect.X += w
        Next
        GDI32.RestoreDC(hDC, hState)
        GDI32.DeleteObject(hRgn)
    End Sub
    Friend Sub RenderShiftSlider(e As RenderParam)
        If Not slider_show Then Exit Sub
        Dim rect As New Rectangle(SM.all_padLeft, SM.all_padTop, Math.Max(MyRect.Width, e.WContent + extraW + 20), slider_size.Height)

        If rect.Height <= 0 Then Exit Sub
        rect.X += e.translate.X
        rect.Y += e.translate.Y

        Dim hDC As IntPtr = e.hDC

        If Not e.IsVisible(rect) Then Exit Sub

        Dim hState As Integer = GDI32.SaveDC(hDC)
        Dim hRgn As IntPtr = GDI32.CreateRectRgn(rect.X, rect.Y - 1, rect.Right, rect.Bottom)
        GDI32.SelectClipRgn(hDC, hRgn)

        If Not SM.offset_mode = OffsetMode.Hidden Then
            rect.X += SM.offset_width
        End If

        Dim box As BoxItem = SM.Box(FocussedBoxIndex)
        Dim w As Integer = box.Width
        rect.Width = w
        For Each i In SM.Box
            If i Is box Then Exit For
            rect.X += i.w
        Next

        Dim rect2 As Rectangle = rect
        rect2.Width = slider_size.Width
        rect2.X += slider_val / 100 * (rect.Width - rect2.Width)
        rect.Height -= 1
        'rect.Width -= 2
        ' rect.X += 1
        Dim sbk As Color = SM.slider_back
        If sbk.IsEmpty Then
            If SM.header_mode = HeaderMode.Hidden Then
                sbk = SM.bkClr
            Else
                sbk = SM.headbkClr
            End If
        End If
        Helper.FillRectangle(hDC, rect, sbk, SM.slider_border)
        If slider_down Then
            Helper.FillRectangle(hDC, rect2, SM.slider_down)
        ElseIf slider_hot Then
            If slider_push Then
                Helper.FillRectangle(hDC, rect2, SM.slider_hot)
            Else
                rect2.Height -= 1
                rect2.Width -= 2
                rect2.X += 1
                Helper.FillRectangle(hDC, rect2, SM.slider_hot, SM.slider_down, 1)
            End If
        Else
            If slider_push Then
                Helper.FillRectangle(hDC, rect2, SM.slider_normal)
            Else
                rect2.Height -= 1
                rect2.Width -= 2
                rect2.X += 1
                Helper.FillRectangle(hDC, rect2, SM.slider_normal, SM.slider_down, 1)
            End If
        End If

        GDI32.RestoreDC(hDC, hState)
        GDI32.DeleteObject(hRgn)
    End Sub
    Friend Sub RenderCollumnHeader(e As RenderParam)
        If SM.header_mode = HeaderMode.Hidden Then Exit Sub
        Dim rect As New Rectangle(SM.all_padLeft, SM.all_padTop, Math.Max(MyRect.Width, e.WContent + extraW + 20), SM.header_height)

        If rect.Height <= 0 Then Exit Sub
        rect.X += e.translate.X
        rect.Y += e.translate.Y

        If slider_show Then
            rect.Height += slider_size.Height
        End If
        Dim hDC As IntPtr = e.hDC

        If Not e.IsVisible(rect) Then Exit Sub

        Dim fd As FontData = Nothing

        Dim hState As Integer = GDI32.SaveDC(hDC)
        Dim hRgn As IntPtr = GDI32.CreateRectRgn(rect.X, rect.Y - 1, rect.Right, rect.Bottom + SM.header_padTop)
        GDI32.SelectClipRgn(hDC, hRgn)

        Dim hBrush As IntPtr = GDI32.CreateSolidBrush(Helper.GetRGB(SM.headbkClr))
        Dim hPen As IntPtr = GDI32.GetStockObject(GDI32.StockObjects.NULL_PEN)

        GDI32.SelectObject(e.hDC, hBrush)
        GDI32.SelectObject(e.hDC, hPen)

        GDI32.Rectangle(hDC, rect.X, rect.Y, rect.Right + 1, rect.Bottom + 1)

        GDI32.DeleteObject(hBrush)
        If Not SM.offset_mode = OffsetMode.Hidden Then
            rect.X += SM.offset_width
        End If
        If slider_show Then rect.Y += slider_size.Height
        rect.Y += SM.header_padTop

        Dim bY As Integer = rect.Y

        Dim sourceBox As BoxItem = SM.Box(FocussedBoxIndex)
        Dim sItem2 As SelectionItem = Me.SL.Curent
        Dim A As Long = 0
        Dim cpr As Integer = GetCharsPerRow(sourceBox)

        If SM.header_mode = HeaderMode.Dynamic Then
            Dim Caret As Long = sItem2.car
            If sItem2.LF Then
                Caret -= 1
            End If
            A = Math.Floor(Caret / cpr) * SM.col_count
        End If
        A += shift_val_pre
        Dim caretColIndex As Integer = Math.Floor((sItem2.Caret Mod cpr) / (sourceBox.trans.CharsPerData + sourceBox.trans.Sparator))
        Dim blendClr As Integer = Helper.GetRGB(Helper.Blend(SM.highLightLineColor, SM.headbkClr, SM.highLightLineColor.A / 255))
        For boxIndex As Integer = 0 To SM.Box.Count - 1
            Dim box As BoxItem = SM.Box(boxIndex)
            Dim SL As SelectionManager = Nothing

            If sourceBox Is box Then
                SL = Me.SL
            Else
                SL = Me.SL.TransformSelection(sourceBox.trans, box.trans)
            End If
            Dim rFontS As Font = SM.fnt

            If Not IsNothing(box.Style) Then
                If Not IsNothing(box.Style.Font) Then
                    rFontS = box.Style.Font
                End If
            End If
            Dim N As Long = 0
            Dim Tx As String = ""
            Dim subTx As String = ""
            Dim Idx As Long = 0
            Dim Iy As Integer = 0

            rect.Y = bY

            Dim perData As Integer = box.trans.CharsPerData + box.trans.Sparator
            Dim colFloor As Integer = Math.Floor(SM.col_count / box.trans.LengthPerData) * box.trans.LengthPerData
            Dim charsPerRow As Integer = (colFloor / box.trans.LengthPerData * perData)
            Dim charsLessPerRow As Integer = (colFloor / box.trans.LengthPerData * perData)
            Dim perLess As Integer = (SM.col_count - colFloor) * 2
            If (SM.col_count - colFloor) > 0 Then perLess += box.trans.Sparator
            charsPerRow += perLess

            Dim rMaps(charsPerRow - 1) As RenderChar
            For i As Integer = 0 To rMaps.Length - 1
                rMaps(i) = New RenderChar
            Next

            Dim rGroups As New List(Of RenderGroup)
            Dim rMap As RenderChar = Nothing
            Dim rGroup As RenderGroup = Nothing
            Dim rX As Single = 0

            Dim SMTextRGB As Integer = Helper.GetRGB(SM.headtxClr)
            Dim SMBackRGB As Integer = Helper.GetRGB(SM.headbkClr)
            Dim isContains As Boolean = False

            Tx = ""
            Dim nHex As Integer = box.trans.CharsPerData
            If nHex > 2 Then
                nHex = 2
            End If
            For i As Integer = 0 To SM.col_count - 1 Step box.trans.LengthPerData
                Tx &= Helper.HexN(i + A, nHex).PadRight(perData)
            Next

            Iy = 0
            Dim iZ As Integer = 0

            For i As Integer = 0 To charsPerRow - 1
                Idx = i

                rMap = rMaps(i)
                rMap.Font = rFontS
                rMap.TextColor = SMTextRGB
                rMap.BackColor = SMBackRGB

                If caretColIndex = Iy And iZ < 2 And SM.highlightLine Then
                    If box Is sourceBox Then rMap.BackColor = blendClr
                End If

                For Each ts In SM.TSS
                    isContains = False
                    If ts.StyleTarget = StyleTarget.All Or ts.StyleTarget = StyleTarget.CollumnHeader Then
                        If ts.Unit = PointUnit.Byte Then
                            If ts.Contains(Iy) Then
                                isContains = True
                            End If
                        Else
                            If ts.Contains(Idx) Then
                                isContains = True
                            End If
                        End If
                    End If

                    If isContains Then
                        If Not ts.TextColor.IsEmpty Then rMap.TextColor = Helper.GetRGB(ts.TextColor)
                        If Not ts.BackColor.IsEmpty Then rMap.BackColor = Helper.GetRGB(ts.BackColor)
                        If Not IsNothing(ts.Font) Then rMap.Font = ts.Font
                    End If
                Next

                iZ += 1
                If iZ >= perData Then
                    iZ = 0
                    Iy += box.trans.LengthPerData
                End If
            Next

            For i As Integer = 0 To charsPerRow - 1
                rMap = rMaps(i)
                If rGroups.Count = 0 Then
                    rGroup = New RenderGroup
                    rGroup.Start = 1
                    rGroup.Length = 1
                    rGroup.Style = rMap
                    rGroups.Add(rGroup)
                Else
                    If rGroup.Style.TextColor = rMap.TextColor And rGroup.Style.BackColor = rMap.BackColor And rGroup.Style.Font Is rMap.Font Then
                        rGroup.Length += 1
                    Else
                        rGroup = New RenderGroup
                        rGroup.Start = i + 1
                        rGroup.Length = 1
                        rGroup.Style = rMap
                        rGroups.Add(rGroup)
                    End If
                End If
            Next

            rX = 0
            Dim rectX As New Rectangle
            For Each i In rGroups
                rectX.X = rect.X + box.xpad + rX
                rectX.Y = rect.Y
                subTx = Mid(Tx, i.Start, i.Length)
                fd = GetFontData(i.Style.Font)
                rectX.Width = Helper.GetTextWidth(fd.ABC, subTx)
                rectX.Height = SM.row_height

                If e.IsVisible(New Rectangle(rectX.X - 1, rectX.Y, rectX.Width, rectX.Height)) Then
                    GDI32.SelectObject(hDC, fd.hFont)
                    GDI32.SetTextColor(hDC, i.Style.TextColor)
                    GDI32.SetBackColor(hDC, i.Style.BackColor)
                    GDI32.TextOut(hDC, rectX.X, rectX.Y, subTx, subTx.Length)
                End If

                rX += rectX.Width
            Next

            For i As Integer = 0 To rMaps.Length - 1
                rMaps(i).Reset()
            Next

            rGroups.Clear()

            rect.X += box.w
        Next
        GDI32.RestoreDC(hDC, hState)
        GDI32.DeleteObject(hRgn)
    End Sub
    Friend Sub RenderOffset(e As RenderParam)
        If SM.offset_mode = OffsetMode.Hidden Then Exit Sub

        Dim rect As New Rectangle(SM.all_padLeft, SM.all_padTop, SM.offset_width, MyRect.Height)

        If rect.Height <= 0 Then Exit Sub
        rect.X += e.translate.X
        rect.Y += e.translate.Y

        Dim hDC As IntPtr = e.hDC

        If Not e.IsVisible(rect) Then Exit Sub

        Dim fd As FontData = Nothing

        If Not SM.header_mode = HeaderMode.Hidden Then
            rect.Y += SM.header_height
        End If
        If slider_show Then
            rect.Y += slider_size.Height
        End If
        Dim hState As Integer = GDI32.SaveDC(hDC)
        Dim hRgn As IntPtr = GDI32.CreateRectRgn(rect.X, rect.Y - 1, rect.Right, rect.Bottom + SM.content_padTop + SM.header_height)
        GDI32.SelectClipRgn(hDC, hRgn)

        Dim hBrush As IntPtr = GDI32.CreateSolidBrush(Helper.GetRGB(SM.offsetbkClr))
        Dim hPen As IntPtr = GDI32.GetStockObject(GDI32.StockObjects.NULL_PEN)

        GDI32.SelectObject(e.hDC, hBrush)
        GDI32.SelectObject(e.hDC, hPen)

        GDI32.Rectangle(hDC, rect.X, rect.Y, rect.X + SM.offset_width + 1, rect.Y + MyRect.Height + 1)

        GDI32.DeleteObject(hBrush)

        'RenderPattern(e, New Rectangle(rect.X + SM.offset_width - SM.offset_wx - 1, rect.Y, SM.offset_wx, MyRect.Height + 1))

        Dim SMTextClr As Integer = Helper.GetRGB(SM.offsettxClr)
        Dim SMBackClr As Integer = Helper.GetRGB(SM.offsetbkClr)
        Dim blendClr As Integer = Helper.GetRGB(Helper.Blend(SM.highLightLineColor, SM.offsetbkClr, SM.highLightLineColor.A / 255))

        hBrush = GDI32.CreateSolidBrush(blendClr)
        hPen = GDI32.GetStockObject(GDI32.StockObjects.NULL_PEN)

        GDI32.SelectObject(e.hDC, hBrush)
        GDI32.SelectObject(e.hDC, hPen)

        If Not SM.header_mode = HeaderMode.Hidden Then
            rect.Y += SM.content_padTop
        End If

        rect.Height = SM.row_height

        Dim drawAddressStart As Long = Me.ShowedStart + shift_val
        Dim drawAddressCount As Integer = Me.ShowedLength

        Dim Tx As String = ""
        Dim subTx As String = ""
        Dim N As Long = 0

        Dim sItem As SelectionItem = Me.SL.Curent
        Dim box As BoxItem = SM.Box(FocussedBoxIndex)
        Dim charsPerRow As Integer = GetCharsPerRow(box)
        Dim caret As Long = sItem.car
        If sItem.LF Then caret -= 1
        Dim line As Long = Math.Floor(caret / charsPerRow)
        Dim index As Long = line * SM.col_count
        Dim isHighLight As Boolean = False
        Dim isContains As Boolean = False
        Dim Idx As Long = 0
        Dim cpr As Integer = SM.offset_mode

        If SM.offsetLiteMode Then
            If SM.offset_mode > 16 Then
                Dim lmax As Integer = 0
                For i As Integer = 0 To ShowedRowCount - 1
                    Dim adr As Long = drawAddressStart + i * SM.col_count
                    Dim adrb As Byte() = BitConverter.GetBytes(adr)
                    Select Case SM.offset_mode
                        Case OffsetMode.Bytes1
                            Tx = adrb.First
                        Case OffsetMode.Bytes2
                            Tx = BitConverter.ToUInt16(adrb, 0)
                        Case OffsetMode.Bytes4
                            Tx = BitConverter.ToUInt32(adrb, 0)
                        Case OffsetMode.Bytes8
                            Tx = adr
                    End Select
                    If Tx.Length > lmax Then
                        lmax = Tx.Length
                    End If
                Next
                cpr = lmax
            Else
                Dim lmax As Integer = 0
                For i As Integer = 0 To ShowedRowCount - 1
                    Dim adr As Long = drawAddressStart + i * SM.col_count
                    Tx = Hex(adr)
                    If Tx.Length > lmax Then
                        lmax = Tx.Length
                    End If
                Next
                cpr = lmax
                If SM.hexSign Then
                    cpr += 1
                End If
            End If

            Dim offsetS(cpr - 1) As Byte

            For i As Integer = 0 To cpr - 1
                offsetS(i) = 48
            Next

            fd = GetFontData(SM.fnt)
            Dim rW As Integer = Helper.GetTextWidthA(fd.ABC, offsetS) + SM.offset_padLeft * 2 + 1
            If Not SM.offset_width = rW Then
                SM.offset_width = rW
                Me.Invalidate()
            End If
        Else
            If SM.offset_mode > 16 Then
                Select Case SM.offset_mode
                    Case OffsetMode.Bytes1
                        cpr = Math.Max(Byte.MinValue.ToString.Length, Byte.MaxValue.ToString.Length)
                    Case OffsetMode.Bytes2
                        cpr = Math.Max(UShort.MinValue.ToString.Length, UShort.MaxValue.ToString.Length)
                    Case OffsetMode.Bytes4
                        cpr = Math.Max(UInteger.MinValue.ToString.Length, UInteger.MaxValue.ToString.Length)
                    Case OffsetMode.Bytes8
                        cpr = Math.Max(Long.MinValue.ToString.Length, Long.MaxValue.ToString.Length)
                End Select
            End If
            If SM.hexSign Then
                cpr += 1
            End If
        End If

        Dim nGroups As New List(Of RenderGroup)
        Dim nGroup As RenderGroup = Nothing

        Dim nMaps(cpr - 1) As RenderChar
        Dim nMap As RenderChar = Nothing
        For i2 As Integer = 0 To nMaps.Length - 1
            nMaps(i2) = New RenderChar
        Next

        drawAddressStart -= shift_val
        For i As Integer = 0 To ShowedRowCount - 1
            If e.IsVisible(rect) Then
                N = (vscroll_val + i) * SM.col_count
                If N < e.AfterLength Or e.AfterLength = 0 And i = 0 Then
                    If SM.offset_mode <= 16 Then
                        If SM.offsetLiteMode Then
                            Tx = Hex(drawAddressStart + i * SM.col_count + shift_val_pre)
                            If Tx.Length > cpr Then Tx = Mid(Tx, Tx.Length - cpr + 2)
                            If SM.hexSign Then
                                Tx &= "h"
                            End If
                            Tx = Tx.PadLeft(cpr)
                        Else
                            Tx = Helper.HexN(drawAddressStart + i * SM.col_count, SM.offset_mode + shift_val_pre)
                            If SM.hexSign Then
                                Tx &= "h"
                            End If
                        End If
                    Else
                        Dim adr As Long = drawAddressStart + i * SM.col_count + shift_val_pre
                        Dim adrb As Byte() = BitConverter.GetBytes(adr)
                        Select Case SM.offset_mode
                            Case OffsetMode.Bytes1
                                Tx = adrb.First
                            Case OffsetMode.Bytes2
                                If (vscroll_val + i) = 0 And Not slider_push And slider_show Then
                                    Tx = BitConverter.ToInt16(adrb, 0)
                                Else
                                    Tx = BitConverter.ToUInt16(adrb, 0)
                                End If
                            Case OffsetMode.Bytes4
                                If (vscroll_val + i) = 0 And Not slider_push And slider_show Then
                                    Tx = BitConverter.ToInt32(adrb, 0)
                                Else
                                    Tx = BitConverter.ToUInt32(adrb, 0)
                                End If
                            Case OffsetMode.Bytes8
                                Tx = adr
                        End Select
                        If Tx.Length > cpr Then Tx = Mid(Tx, Tx.Length - cpr + 1)
                        Tx = Tx.PadLeft(cpr)
                    End If

                    isHighLight = N <= index And index < (N + SM.col_count)
                    isHighLight = isHighLight And SM.highlightLine

                    If isHighLight Then
                        For i2 As Integer = 0 To cpr - 1
                            nMap = nMaps(i2)
                            nMap.TextColor = SMTextClr
                            nMap.BackColor = blendClr
                            nMap.Font = SM.fnt
                        Next
                    Else
                        For i2 As Integer = 0 To cpr - 1
                            nMap = nMaps(i2)
                            nMap.TextColor = SMTextClr
                            nMap.BackColor = SMBackClr
                            nMap.Font = SM.fnt
                        Next

                    End If

                    For i2 As Integer = 0 To nMaps.Length - 1
                        Idx = (vscroll_val + i) * cpr + i2
                        nMap = nMaps(i2)
                        For Each ts In SM.TSS
                            isContains = False
                            If ts.StyleTarget = StyleTarget.All Or ts.StyleTarget = StyleTarget.Offset Then
                                If ts.Unit = PointUnit.Byte Then
                                    If ts.Contains(N) Then
                                        isContains = True
                                    End If
                                Else
                                    If ts.Contains(Idx) Then
                                        isContains = True
                                    End If
                                End If
                            End If

                            If isContains Then
                                If Not ts.TextColor.IsEmpty Then nMap.TextColor = Helper.GetRGB(ts.TextColor)
                                If Not ts.BackColor.IsEmpty Then
                                    If isHighLight Then
                                        nMap.BackColor = Helper.GetRGB(Helper.Blend(ts.BackColor, SM.offsetbkClr, SM.highLightLineColor.A / 255))
                                    Else
                                        nMap.BackColor = Helper.GetRGB(ts.BackColor)
                                    End If
                                End If
                                If Not IsNothing(ts.Font) Then nMap.Font = ts.Font
                            End If
                        Next
                    Next

                    For i2 As Integer = 0 To nMaps.Length - 1
                        nMap = nMaps(i2)
                        If nGroups.Count = 0 Then
                            nGroup = New RenderGroup
                            nGroup.Start = 1
                            nGroup.Length = 1
                            nGroup.Style = nMap
                            nGroups.Add(nGroup)
                        Else
                            If nGroup.Style.TextColor = nMap.TextColor And nGroup.Style.BackColor = nMap.BackColor And nGroup.Style.Font Is nMap.Font Then
                                nGroup.Length += 1
                            Else
                                nGroup = New RenderGroup
                                nGroup.Start = i2 + 1
                                nGroup.Length = 1
                                nGroup.Style = nMap
                                nGroups.Add(nGroup)
                            End If
                        End If
                    Next

                    If isHighLight Then GDI32.Rectangle(hDC, rect.X, rect.Y, rect.X + SM.offset_width + 1, rect.Y + SM.row_height + 1)

                    Dim w As Single = 0
                    Dim rX As Single = 0
                    For Each i2 In nGroups
                        subTx = Mid(Tx, i2.Start, i2.Length)
                        fd = GetFontData(i2.Style.Font)
                        w = Helper.GetTextWidth(fd.ABC, subTx)

                        GDI32.SelectObject(hDC, fd.hFont)
                        GDI32.SetTextColor(hDC, i2.Style.TextColor)
                        GDI32.SetBackColor(hDC, i2.Style.BackColor)
                        GDI32.TextOut(hDC, rect.X + SM.offset_padLeft + rX, rect.Y, subTx, i2.Length)
                        rX += w
                    Next


                End If
            End If

            For i2 As Integer = 0 To nMaps.Length - 1
                nMaps(i2).Reset()
            Next
            nGroups.Clear()

            rect.Y += SM.row_height
        Next

        GDI32.DeleteObject(hBrush)
        GDI32.RestoreDC(hDC, hState)
        GDI32.DeleteObject(hRgn)
    End Sub
    Friend Sub RenderPattern(e As RenderParam, ByVal rect As Rectangle)
        Dim bmp As New Bitmap(SM.offset_wx, 2)
        Dim ax As Boolean = False
        For y As Integer = 0 To bmp.Height - 1
            For x As Integer = 0 To bmp.Width - 1
                If Not ax Then
                    bmp.SetPixel(x, y, SM.offsetbkClr)
                Else
                    bmp.SetPixel(x, y, Color.Purple)
                End If
                ax = Not ax
            Next
            ax = Not ax
        Next

        Dim hbmp As IntPtr = bmp.GetHbitmap
        Dim hPatternBrush As IntPtr = GDI32.CreatePatternBrush(hbmp)
        Dim hPen As IntPtr = GDI32.GetStockObject(GDI32.StockObjects.NULL_PEN)

        Dim hBrushPrevious As IntPtr = GDI32.SelectObject(e.hDC, hPatternBrush)
        Dim hPenPrevious As IntPtr = GDI32.SelectObject(e.hDC, hPen)
        GDI32.Rectangle(e.hDC, rect.Left, rect.Top, rect.Right, rect.Bottom)
        GDI32.SelectObject(e.hDC, hBrushPrevious)
        GDI32.SelectObject(e.hDC, hPenPrevious)
        GDI32.DeleteObject(hPatternBrush)
        bmp.Dispose()
    End Sub
    Friend Sub RenderContents(e As RenderParam)
        Dim rect As New Rectangle(SM.all_padLeft, SM.all_padTop, e.WContent, MyRect.Height)
        If Not SM.offset_mode = OffsetMode.Hidden Then
            rect.X += SM.offset_width
        End If

        If rect.Height <= 0 Then Exit Sub
        rect.X += e.translate.X
        rect.Y += e.translate.Y

        Dim hDC As IntPtr = e.hDC

        If Not e.IsVisible(rect) Then Exit Sub

        Dim fd As FontData = Nothing
        If Not SM.header_mode = HeaderMode.Hidden Then
            rect.Y += SM.header_height + SM.content_padTop
        End If
        If slider_show Then
            rect.Y += slider_size.Height
        End If

        Dim hState As Integer = GDI32.SaveDC(hDC)
        Dim hRgn As IntPtr = GDI32.CreateRectRgn(rect.X, rect.Y - 1, rect.Right, rect.Bottom)
        GDI32.SelectClipRgn(hDC, hRgn)

        rect.Height = SM.row_height

        Dim bY As Integer = rect.Y

        Dim sourceBox As BoxItem = SM.Box(FocussedBoxIndex)
        Dim sItem2 As SelectionItem = Me.SL.Curent
        Dim caretz As Long = sItem2.car
        If sItem2.LF Then caretz -= 1
        Dim line As Long = Math.Floor(caretz / GetCharsPerRow(sourceBox))
        Dim index As Long = line * SM.col_count
        Dim isHighLight As Boolean = False

        Dim myFocus As Boolean = Me.Focused

        If Not myFocus Then
            indicator_blink = False
        End If

        Dim N As Long
        Dim Ix As Integer
        Dim Tx As String = ""
        Dim subTx As String = ""
        Dim Idx As Long
        Dim box As BoxItem
        Dim rb As RenderBox
        Dim rMap As RenderChar
        Dim rMaps() As RenderChar
        Dim rGroups As New List(Of RenderGroup)
        Dim rGroup As RenderGroup = Nothing
        Dim rX As Single
        Dim SL As SelectionManager
        Dim isIndicator As Boolean

        For boxIndex As Integer = 0 To SM.Box.Count - 1
            rect.Y = bY
            box = SM.Box(boxIndex)
            rb = GetBoxStyles(boxIndex)
            SL = rb.SL
            isIndicator = False
            For y As Integer = 0 To ShowedRowCount - 1
                Ix = (e.A - QR.Position) + y * SM.col_count

                N = (vscroll_val + y) * SM.col_count
                isHighLight = N <= index And index < (N + SM.col_count)
                isHighLight = isHighLight And SM.highlightLine

                Tx = GetRowText(vscroll_val + y, box.trans, True)
                rMaps = GetRowStyles(vscroll_val + y, rb)

                For i As Integer = 0 To rb.CharsPerRow - 1
                    rMap = rMaps(i)
                    If rGroups.Count = 0 Then
                        rGroup = New RenderGroup
                        rGroup.Start = 1
                        rGroup.Length = 1
                        rGroup.Style = rMap
                        rGroups.Add(rGroup)
                    Else
                        If rGroup.Style.TextColor = rMap.TextColor And rGroup.Style.BackColor = rMap.BackColor And rGroup.Style.Font Is rMap.Font And rGroup.Style.IsPartial = rMap.IsPartial Then
                            rGroup.Length += 1
                        Else
                            rGroup = New RenderGroup
                            rGroup.Start = i + 1
                            rGroup.Length = 1
                            rGroup.Style = rMap
                            rGroups.Add(rGroup)
                        End If
                    End If
                Next

                If isHighLight Then
                    Helper.FillRectangle(hDC, New Rectangle(rect.X, rect.Y, box.w, SM.row_height), rb.SMBackBlendRGB)
                End If

                Dim rectX As New Rectangle
                Dim paintDefault As Boolean = True
                If SM.usrpaint_content Then
                    rectX.X = rect.X + box.xpad
                    rectX.Y = rect.Y
                    rectX.Width = box.w
                    rectX.Height = SM.row_height
                    Dim b(SM.col_count - 1) As Byte
                    Helper.CopyBytes(QR.Buffer, Ix, b, 0, SM.col_count)
                    rectX.X -= e.translate.X
                    rectX.Y -= e.translate.Y

                    For Each i In rGroups
                        subTx = Mid(Tx, i.Start, i.Length)
                        fd = GetFontData(i.Style.Font)
                        i.Width = Helper.GetTextWidth(fd.ABC, subTx)
                        i.Height = SM.row_height
                    Next

                    Dim evt As New ContentPaintEventArgs(hDC, rectX, e.translate, vscroll_val + y, Tx, b, rGroups)
                    RaiseEvent ContentPaint(Me, evt)
                    paintDefault = evt.DefaultPaint
                End If

                If paintDefault Then
                    rX = 0
                    For Each i In rGroups
                        rectX.X = rect.X + box.xpad + rX
                        rectX.Y = rect.Y
                        rectX.Height = SM.row_height
                        subTx = Mid(Tx, i.Start, i.Length)
                        fd = GetFontData(i.Style.Font)

                        Dim uniByt As Byte() = Helper.uni.GetBytes(subTx)
                        rectX.Width = Helper.GetTextWidthW(fd.ABC, uniByt)

                        Dim rY As Single = 0
                        If Not i.Style.Font Is SM.fnt Then
                            Dim h As Single = 0
                            Helper.FillRectangle(hDC, New Rectangle(rectX.X - 1, rectX.Y, rectX.Width, rectX.Height), i.Style.BackColor)
                            h = Helper.GetTextHeight(fd.hFont)
                            rY = Math.Floor((SM.row_height - h) / 2)
                            If rY < 0 Then rY = 0
                        End If

                        If e.IsVisible(New Rectangle(rectX.X - 1, rectX.Y, rectX.Width, rectX.Height)) Then
                            GDI32.SelectObject(hDC, fd.hFont)
                            GDI32.SetTextColor(hDC, i.Style.TextColor)
                            GDI32.SetBackColor(hDC, i.Style.BackColor)
                            GDI32.TextOutW2(hDC, rectX.X, rectX.Y + rY, uniByt(0), i.Length)
                        End If

                        rX += rectX.Width
                    Next
                End If

                If Not isIndicator And myFocus And Not ondrag Then
                    If Not (SM.wMode = WriteMode.Overwrite And SL.SelectionLength = 0) Then
                        If indicator_blink Or Not FocussedBoxIndex = boxIndex Then
                            rX = -1
                            Dim px As Integer = 0
                            Dim pxByt As Byte() = Nothing
                            Dim w As Integer = 0
                            Dim h As Integer = 0
                            Dim LFx As Integer = 0
                            Dim sItem As SelectionItem = SL.Curent

                            For i As Integer = 0 To rb.CharsPerRow - 1
                                Idx = (e.A + y * SM.col_count) / SM.col_count * rb.CharsPerRow + i
                                rMap = rMaps(i)
                                fd = GetFontData(rMap.Font)
                                subTx = Mid(Tx, i + 1, 1)
                                w = Helper.GetTextWidth(fd.ABC, subTx)
                                Dim gobak As Boolean = False
                                If Not FocussedBoxIndex = boxIndex Then
                                    If i = rb.CharsPerRow - 1 And Not sItem.LF And SL.IsSelected(Idx) Then
                                        Dim res As Long = sItem2.car Mod GetCharsPerRow(sourceBox)
                                        If Not (res = 0 And Not sItem2.LF) And sItem2.car > sItem2.anc Then
                                            sItem.LF = True
                                            gobak = True
                                        End If
                                    End If
                                End If
                                If SL.IsCaret(Idx) Then
                                    If sItem.LF Then
                                        LFx = w + 1
                                    Else
                                        LFx = 0
                                    End If

                                    h = SM.row_height ' Helper.GetTextHeight(fd.hFont)
                                    For i2 As Integer = 0 To h - 1
                                        For xx As Integer = 0 To SM.indicatorSize - 1
                                            px = GDI32.GetPixel(hDC, rect.X + box.xpad + rX + LFx + xx, rect.Y + i2)
                                            pxByt = BitConverter.GetBytes(px)
                                            pxByt(0) = 255 - pxByt(0)
                                            pxByt(1) = 255 - pxByt(1)
                                            pxByt(2) = 255 - pxByt(2)
                                            px = BitConverter.ToInt32(pxByt, 0)
                                            GDI32.SetPixel(hDC, rect.X + box.xpad + rX + LFx + xx, rect.Y + i2, px)
                                        Next
                                    Next
                                    isIndicator = True
                                    If gobak Then
                                        sItem.LF = False
                                    End If
                                    Exit For
                                End If
                                If gobak Then
                                    sItem.LF = False
                                End If
                                rX += w
                            Next
                        End If
                    ElseIf SM.wMode = WriteMode.Overwrite And SL.SelectionLength = 0 And (SM.ovrMode.HasFlag(OverModes.Line) Or FocussedBoxIndex = boxIndex) Then
                        rX = -1
                        Dim px As Integer = 0
                        Dim pxByt As Byte() = Nothing
                        Dim w As Integer = 0
                        Dim h As Integer = 0
                        Dim IsOvered As Boolean = False
                        For i As Integer = 0 To rb.CharsPerRow - 1
                            Idx = (e.A + y * SM.col_count) / SM.col_count * rb.CharsPerRow + i
                            rMap = rMaps(i)
                            fd = GetFontData(rMap.Font)
                            w = Helper.GetTextWidth(fd.ABC, Mid(Tx, i + 1, 1))
                            If FocussedBoxIndex = boxIndex Then
                                If indicator_blink Then
                                    If SL.IsCaret(Idx) Then
                                        h = SM.row_height ' Helper.GetTextHeight(fd.hFont)
                                        For i2 As Integer = 0 To w - 1
                                            For xx As Integer = 0 To SM.indicatorSize - 1
                                                px = GDI32.GetPixel(hDC, rect.X + box.xpad + rX + i2, rect.Y + h - 1 - xx)
                                                pxByt = BitConverter.GetBytes(px)
                                                pxByt(0) = 255 - pxByt(0)
                                                pxByt(1) = 255 - pxByt(1)
                                                pxByt(2) = 255 - pxByt(2)
                                                px = BitConverter.ToInt32(pxByt, 0)
                                                GDI32.SetPixel(hDC, rect.X + box.xpad + rX + i2, rect.Y + h - 1 - xx, px)
                                            Next
                                        Next

                                        isIndicator = True
                                        Exit For
                                    End If
                                End If
                            Else
                                If SL.IsOver(Idx, rb.TI) Then
                                    h = SM.row_height ' Helper.GetTextHeight(fd.hFont)
                                    For i2 As Integer = 0 To w - 1
                                        For xx As Integer = 0 To SM.indicatorSize - 1
                                            px = GDI32.GetPixel(hDC, rect.X + box.xpad + rX + i2, rect.Y + h - 1 - xx)
                                            pxByt = BitConverter.GetBytes(px)
                                            pxByt(0) = 255 - pxByt(0)
                                            pxByt(1) = 255 - pxByt(1)
                                            pxByt(2) = 255 - pxByt(2)

                                            Dim clr As Color = Color.FromArgb(255, pxByt(0), pxByt(1), pxByt(2))
                                            clr = Helper.Blend(Color.Gray, clr, 0.5)
                                            pxByt(0) = clr.R
                                            pxByt(1) = clr.G
                                            pxByt(2) = clr.B

                                            px = BitConverter.ToInt32(pxByt, 0)
                                            GDI32.SetPixel(hDC, rect.X + box.xpad + rX + i2, rect.Y + h - 1 - xx, px)
                                        Next
                                    Next
                                    isIndicator = True
                                    IsOvered = True
                                ElseIf IsOvered Then
                                    Exit For
                                End If
                            End If
                            rX += w
                        Next
                    End If
                End If

                If ondrag And boxIndex = dragBoxIndex Then
                    rX = -1
                    Dim px As Integer = 0
                    Dim pxByt As Byte() = Nothing
                    Dim w As Integer = 0
                    Dim h As Integer = 0
                    Dim LFx As Integer = 0

                    For i As Integer = 0 To rb.CharsPerRow - 1
                        Idx = (e.A + y * SM.col_count) / SM.col_count * rb.CharsPerRow + i
                        rMap = rMaps(i)
                        fd = GetFontData(rMap.Font)
                        subTx = Mid(Tx, i + 1, 1)
                        w = Helper.GetTextWidth(fd.ABC, subTx)
                        If Idx = dragCaret Then
                            Dim isSelected As Boolean = False
                            For Each itm In SL.Items
                                Dim smin As Long = itm.anc + 1
                                Dim smax As Long = itm.car
                                If smin > smax Then
                                    smin = itm.car
                                    smax = itm.anc
                                End If
                                If smin <= dragCaret And dragCaret < smax Then
                                    isSelected = True
                                    Exit For
                                End If
                            Next

                            If Not isSelected Then
                                If dragLF Then
                                    LFx = w + 1
                                Else
                                    LFx = 0
                                End If
                                h = SM.row_height ' Helper.GetTextHeight(fd.hFont)
                                Dim sWidth As Integer = 2
                                If sWidth > SM.indicatorSize + 1 Then sWidth = SM.indicatorSize + 1

                                For i2 As Integer = 0 To h - 1
                                    For xx As Integer = 0 To sWidth - 1
                                        px = GDI32.GetPixel(hDC, rect.X + box.xpad + rX + LFx + xx, rect.Y + i2)
                                        pxByt = BitConverter.GetBytes(px)
                                        pxByt(0) = 255 - pxByt(0)
                                        pxByt(1) = 255 - pxByt(1)
                                        pxByt(2) = 255 - pxByt(2)
                                        px = BitConverter.ToInt32(pxByt, 0)
                                        GDI32.SetPixel(hDC, rect.X + box.xpad + rX + LFx + xx, rect.Y + i2, px)
                                    Next
                                Next
                            End If
                            Exit For
                        End If
                        rX += w
                    Next
                End If

                rGroups.Clear()
                rect.Y += SM.row_height
            Next

            rect.X += box.w
        Next
        GDI32.RestoreDC(hDC, hState)
        GDI32.DeleteObject(hRgn)
    End Sub
    Friend Sub RenderBorder(e As RenderParam)
        Dim width As Integer = SM.borderW
        Dim rect As New Rectangle(SM.all_padLeft, SM.all_padTop, Math.Max(MyRect.Width, e.WContent + extraW + 20) + width, MyRect.Height)

        If rect.Height <= 0 Then Exit Sub
        rect.X += e.translate.X
        rect.Y += e.translate.Y

        Dim hDC As IntPtr = e.hDC

        If Not e.IsVisible(rect) Then Exit Sub

        Dim hState As Integer = GDI32.SaveDC(hDC)
        Dim hRgn As IntPtr = GDI32.CreateRectRgn(rect.X, rect.Y - 1, rect.Right, rect.Bottom)
        GDI32.SelectClipRgn(hDC, hRgn)

        Dim hBrush As IntPtr = GDI32.CreateSolidBrush(Helper.GetRGB(SM.lineColor))
        Dim hPen As IntPtr = GDI32.CreatePen(GDI32.PenStyle.PS_SOLID, 1, Helper.GetRGB(SM.lineColor))

        GDI32.SelectObject(e.hDC, hBrush)
        GDI32.SelectObject(e.hDC, hPen)

        If Not SM.header_mode = HeaderMode.Hidden Then
            Dim yy As Integer = 0
            If slider_show Then yy = slider_size.Height
            GDI32.Rectangle(hDC, rect.X, rect.Y + SM.header_height + yy, rect.Right, rect.Y + SM.header_height + width + yy)
        End If

        If Not SM.offset_mode = OffsetMode.Hidden Then
            GDI32.Rectangle(hDC, rect.X + SM.offset_width, rect.Y, rect.X + SM.offset_width + width, rect.Bottom)
        End If

        If Not SM.offset_mode = OffsetMode.Hidden Then
            rect.X += SM.offset_width
        End If

        For Each box In SM.Box
            rect.X += box.w
            GDI32.Rectangle(hDC, rect.X, rect.Y, rect.X + width, rect.Bottom)
        Next

        GDI32.DeleteObject(hBrush)
        GDI32.RestoreDC(hDC, hState)
        GDI32.DeleteObject(hRgn)
    End Sub
    Friend Sub RenderTimeText(e As RenderParam)
        Dim rect As New Rectangle(SM.all_padLeft, SM.all_padTop, MyRect.Width, SM.row_height)

        If rect.Height <= 0 Then Exit Sub
        rect.Y += e.translate.Y

        Dim hDC As IntPtr = e.hDC

        Dim hState As Integer = GDI32.SaveDC(hDC)
        Dim hRgn As IntPtr = GDI32.CreateRectRgn(rect.X, rect.Y - 1, rect.Right, rect.Bottom)
        GDI32.SelectClipRgn(hDC, hRgn)

        Dim fd As FontData = GetFontData(SM.fnt)
        GDI32.SelectObject(hDC, fd.hFont)
        GDI32.SetTextColor(hDC, 0)
        GDI32.SetBackColor(hDC, &HFFFFFF)

        Dim avg As Long = 0
        For Each i In renderTime
            avg += i
        Next
        avg = avg / renderTime.Length

        For i As Integer = renderTimeAvg.Length - 1 To 1 Step -1
            renderTimeAvg(i) = renderTimeAvg(i - 1)
        Next
        renderTimeAvg(0) = avg

        Dim rMax As Long = 0
        Dim rMin As Long = renderTimeAvg(0)
        For Each i In renderTimeAvg
            rMax = Math.Max(rMax, i)
            rMin = Math.Min(rMin, i)
        Next

        Dim tx As String = "Avg(" & avg & ") Min(" & rMin & ") Max(" & rMax & ")"
        GDI32.TextOut(hDC, rect.X, rect.Y, tx, tx.Length)

        GDI32.RestoreDC(hDC, hState)
        GDI32.DeleteObject(hRgn)
    End Sub
    Friend Function ReplaceChars(ByVal text As String) As String
        For Each i In nodrawable_chars
            text = text.Replace(i, ".")
        Next
        Return text
    End Function
    Friend Shared Function GetFontData(ByVal Font As Font) As FontData
        Return StyleSet.GetFontData(Font)
    End Function
    Friend Sub AddWrongFormatted(Box As BoxItem, Position As Long, Text As String)
        For Each i In WF
            If i.Position = Position Then
                If i.Box Is Box Then
                    i.Text = Text
                    Exit Sub
                End If
            End If
        Next
        WF.Add(New WrongFormatted(Box, Position, Text))
    End Sub
    Public Property UnAceessableSymbol As Char
        Get
            Return ubSymbol
        End Get
        Set(value As Char)
            If value = "" Or IsNothing(value) Then
                value = " "
            End If
            ubSymbol = value
        End Set
    End Property
    Public ReadOnly Property UnAceessable As UnAccessableByte
        Get
            Return ub
        End Get
    End Property
    Public ReadOnly Property UnEditable As UnEditableByte
        Get
            Return ue
        End Get
    End Property
    Friend Sub FindSameText(ByVal line As Long)
        Dim Idx As Integer
        Dim dataLen As Integer
        Dim trans As ITransformer = SM.Box(0).trans
        Dim colFloor As Integer = Math.Floor(SM.col_count / trans.LengthPerData) * trans.LengthPerData
        Dim perData As Integer = trans.CharsPerData + trans.Sparator
        Dim lineIndex As Long = line * SM.col_count
        Dim thePos As Long
        Dim isWrong As Boolean
        Dim wrItem As WrongFormatted = Nothing
        Dim isUB As Boolean
        Dim subTx As String = ""

        For i As Integer = 0 To SM.col_count - 1 Step trans.LengthPerData
            thePos = lineIndex + i
            If thePos < shift_val_pre Then
                subTx = ""
            Else
                Idx = thePos - QR.Position - shift_val_pre

                If i < colFloor Then
                    dataLen = trans.LengthPerData
                Else
                    dataLen = SM.col_count - i
                End If

                If Idx >= QR.Length Then
                    dataLen = 0
                ElseIf Idx < QR.Length And QR.Length < Idx + trans.LengthPerData Then
                    dataLen = QR.Length - Idx
                End If

                isWrong = False
                isUB = ub.IsInRange(thePos, dataLen)

                If isUB Then dataLen = 0

                If Not isUB Then
                    For Each i2 In WF
                        If i2.Position = thePos And i2.Box.trans Is trans Then
                            isWrong = True
                            wrItem = i2
                            Exit For
                        End If
                    Next
                End If

                subTx = ""
                If dataLen = trans.LengthPerData Then
                    If isWrong Then
                        subTx = Mid(wrItem.Text, 1, perData)
                    Else
                        subTx = trans.GetString(QR.Buffer, Idx)
                    End If
                ElseIf dataLen = 0 Then
                    If isUB Then
                        subTx = "".PadRight(trans.CharsPerData, ubSymbol)
                    Else
                        subTx = ""
                    End If
                Else
                    If isWrong Then
                        subTx = Mid(wrItem.Text, 1, perData)
                    Else
                        For i2 As Integer = 0 To dataLen - 1
                            subTx &= Helper.Hex2(QR.Buffer(Idx + i2))
                        Next
                    End If
                End If

                If hotData = subTx Then
                    hotFind.Add(thePos)
                End If
            End If
        Next
    End Sub

    Friend Function GetRowText(ByVal line As Long, ByVal trans As ITransformer, ByVal replaced As Boolean) As String
        Return GetRowText(QR, line, trans, replaced)
    End Function
    Friend Function GetRowText(ByVal QR As QuickReader, ByVal line As Long, ByVal trans As ITransformer, ByVal replaced As Boolean) As String
        Dim box As BoxItem = Nothing
        For Each i In SM.Box
            If i.trans Is trans Then
                box = i
                Exit For
            End If
        Next
        Return GetRowText(QR, line, box, replaced)
    End Function
    Friend Function GetRowText(ByVal QR As QuickReader, ByVal line As Long, ByVal box As BoxItem, ByVal replaced As Boolean) As String
        Dim Idx As Integer
        Dim Tx As String = ""
        Dim dataLen As Integer
        Dim trans As ITransformer = box.trans
        Dim colFloor As Integer = Math.Floor(SM.col_count / trans.LengthPerData) * trans.LengthPerData
        Dim perData As Integer = trans.CharsPerData + trans.Sparator
        Dim lineIndex As Long = line * SM.col_count
        Dim thePos As Long
        Dim isWrong As Boolean
        Dim wrItem As WrongFormatted = Nothing
        Dim isUB As Boolean
        Dim subTx As String = ""
        Dim firstTrans As Boolean = SM.Box(0).trans Is trans

        For i As Integer = 0 To SM.col_count - 1 Step trans.LengthPerData
            thePos = lineIndex + i
            If thePos < shift_val_pre Then
                subTx = "".PadRight(perData)
                Tx &= subTx
            Else
                Idx = thePos - QR.Position - shift_val_pre

                If i < colFloor Then
                    dataLen = trans.LengthPerData
                Else
                    dataLen = SM.col_count - i
                End If

                If Idx >= QR.Length Then
                    dataLen = 0
                ElseIf Idx < QR.Length And QR.Length < Idx + trans.LengthPerData Then
                    dataLen = QR.Length - Idx
                End If

                isWrong = False
                isUB = ub.IsInRange(thePos, dataLen)

                If isUB Then dataLen = 0

                If Not isUB Then
                    For Each i2 In WF
                        If i2.Position = thePos And i2.Box Is box Then
                            isWrong = True
                            wrItem = i2
                            Exit For
                        End If
                    Next
                End If

                subTx = ""
                If dataLen = trans.LengthPerData Then
                    If isWrong Then
                        subTx = Mid(wrItem.Text, 1, perData)
                    Else
                        If replaced Then
                            subTx = ReplaceChars(trans.GetString(QR.Buffer, Idx))
                        Else
                            subTx = trans.GetString(QR.Buffer, Idx)
                        End If
                    End If
                ElseIf dataLen = 0 Then
                    If isUB Then
                        subTx = "".PadRight(trans.CharsPerData, ubSymbol)
                    Else
                        subTx = ""
                    End If
                Else
                    If isWrong Then
                        subTx = Mid(wrItem.Text, 1, perData)
                    Else
                        For i2 As Integer = 0 To dataLen - 1
                            subTx &= Helper.Hex2(QR.Buffer(Idx + i2))
                        Next
                    End If
                End If
                subTx = subTx.PadRight(perData)
                If subTx.Length > perData Then
                    subTx = Mid(subTx, 1, perData)
                End If
                Tx &= subTx
            End If

        Next
        Return Tx
    End Function
    Friend Function GetBoxStyles(ByVal boxIndex As Integer) As RenderBox
        Dim box As BoxItem = SM.Box(boxIndex)
        Dim trans As ITransformer = box.trans
        Dim rb As New RenderBox
        With rb
            .Box = box
            .BoxIndex = boxIndex
            .afterLength = bb.GetLength + shift_val_pre
            .TI = Transformers.GetAdvancedTransformInfo(trans, SM.col_count, .afterLength)
            .perData = trans.CharsPerData + trans.Sparator
            .colFloor = Math.Floor(SM.col_count / trans.LengthPerData) * trans.LengthPerData
            .CharsPerRow = (.colFloor / trans.LengthPerData * .perData)
            .partialChars = (SM.col_count - .colFloor) * 2
            .CharsPerRow += .partialChars

            If SelectedBox Is box Then
                .SL = Me.SL
            Else
                .SL = Me.SL.CreateTransform(box.trans)
            End If

            If IsNothing(box.Style) Then
                .SMBackRGB = Helper.GetRGB(SM.bkClr)
                .SMTextRGB = Helper.GetRGB(Helper.Blend(SM.txClr, SM.bkClr, SM.txClr.A / 255))
                .SMSelTextRGB = Helper.GetRGB(Helper.Blend(SM.seltxClr, SM.bkClr, SM.seltxClr.A / 255))
                .SMSelBackRGB = Helper.GetRGB(Helper.Blend(SM.selbkClr, SM.bkClr, SM.selbkClr.A / 255))
                .SMOvrTextRGB = Helper.GetRGB(Helper.Blend(SM.ovrtxClr, SM.bkClr, SM.ovrtxClr.A / 255))
                .SMOvrBackRGB = Helper.GetRGB(Helper.Blend(SM.ovrbkClr, SM.bkClr, SM.ovrbkClr.A / 255))
                .SMOvrText2RGB = Helper.GetRGB(Helper.Blend(SM.ovrtx2Clr, SM.bkClr, SM.ovrtx2Clr.A / 255))
                .SMOvrBack2RGB = Helper.GetRGB(Helper.Blend(SM.ovrbk2Clr, SM.bkClr, SM.ovrbk2Clr.A / 255))
                .SMOvrText3RGB = Helper.GetRGB(Helper.Blend(SM.ovrtx3Clr, SM.bkClr, SM.ovrtx3Clr.A / 255))
                .SMOvrBack3RGB = Helper.GetRGB(Helper.Blend(SM.ovrbk3Clr, SM.bkClr, SM.ovrbk3Clr.A / 255))
                .SMGrayTextRGB = Helper.GetRGB(Helper.Blend(SM.graytxClr, SM.bkClr, SM.graytxClr.A / 255))
                .SMGrayBackRGB = Helper.GetRGB(Color.Black)
                .SMWrongTextRGB = Helper.GetRGB(SM.wrongtxClr)
                .SMUBTextRGB = Helper.GetRGB(SM.ubtxClr)
                .SMUETextRGB = Helper.GetRGB(SM.uetxClr)
                .SMBackBlendRGB = Helper.GetRGB(Helper.Blend(SM.highLightLineColor, SM.bkClr, SM.highLightLineColor.A / 255))
                .rBackS = SM.bkClr
                .rSelS = SM.selbkClr
                .rFontS = SM.Font
            Else
                Dim stsSource As BaseStyle = box.Style
                Dim sts As New BaseStyle
                If stsSource.BackColor.IsEmpty Then
                    sts.BackColor = SM.bkClr
                Else
                    sts.BackColor = stsSource.BackColor
                End If

                If stsSource.TextColor.IsEmpty Then
                    sts.TextColor = SM.txClr
                Else
                    sts.TextColor = stsSource.TextColor
                End If

                If stsSource.HightlightTextColor.IsEmpty Then
                    sts.HightlightTextColor = SM.seltxClr
                Else
                    sts.HightlightTextColor = stsSource.HightlightTextColor
                End If

                If stsSource.HightlightBackColor.IsEmpty Then
                    sts.HightlightBackColor = SM.selbkClr
                Else
                    sts.HightlightBackColor = stsSource.HightlightBackColor
                End If

                If stsSource.HotBackColor.IsEmpty Then
                    sts.HotBackColor = SM.ovrbkClr
                Else
                    sts.HotBackColor = stsSource.HotBackColor
                End If

                If stsSource.HotTextColor.IsEmpty Then
                    sts.HotTextColor = SM.ovrtxClr
                Else
                    sts.HotTextColor = stsSource.HotTextColor
                End If

                If stsSource.HotBackColor2.IsEmpty Then
                    sts.HotBackColor2 = SM.ovrbk2Clr
                Else
                    sts.HotBackColor2 = stsSource.HotBackColor2
                End If

                If stsSource.HotTextColor2.IsEmpty Then
                    sts.HotTextColor2 = SM.ovrtx2Clr
                Else
                    sts.HotTextColor2 = stsSource.HotTextColor2
                End If

                If stsSource.HotBackColor3.IsEmpty Then
                    sts.HotBackColor3 = SM.ovrbk3Clr
                Else
                    sts.HotBackColor3 = stsSource.HotBackColor3
                End If

                If stsSource.HotTextColor3.IsEmpty Then
                    sts.HotTextColor3 = SM.ovrtx3Clr
                Else
                    sts.HotTextColor3 = stsSource.HotTextColor3
                End If

                If stsSource.GrayTextColor.IsEmpty Then
                    sts.GrayTextColor = SM.graytxClr
                Else
                    sts.GrayTextColor = stsSource.GrayTextColor
                End If
                If IsNothing(stsSource.Font) Then
                    sts.Font = SM.Font
                Else
                    sts.Font = stsSource.Font
                End If
                .rFontS = sts.Font

                .SMBackRGB = Helper.GetRGB(sts.BackColor)
                .SMTextRGB = Helper.GetRGB(Helper.Blend(sts.TextColor, sts.BackColor, sts.TextColor.A / 255))
                .SMSelTextRGB = Helper.GetRGB(Helper.Blend(sts.HightlightTextColor, sts.BackColor, sts.HightlightTextColor.A / 255))
                .SMSelBackRGB = Helper.GetRGB(Helper.Blend(sts.HightlightBackColor, sts.BackColor, sts.HightlightBackColor.A / 255))
                .SMOvrTextRGB = Helper.GetRGB(Helper.Blend(sts.HotTextColor, sts.BackColor, sts.HotTextColor.A / 255))
                .SMOvrBackRGB = Helper.GetRGB(Helper.Blend(sts.HotBackColor, sts.BackColor, sts.HotBackColor.A / 255))
                .SMOvrText2RGB = Helper.GetRGB(Helper.Blend(sts.HotTextColor2, sts.BackColor, sts.HotTextColor2.A / 255))
                .SMOvrBack2RGB = Helper.GetRGB(Helper.Blend(sts.HotBackColor2, sts.BackColor, sts.HotBackColor2.A / 255))
                .SMOvrText3RGB = Helper.GetRGB(Helper.Blend(sts.HotTextColor3, sts.BackColor, sts.HotTextColor3.A / 255))
                .SMOvrBack3RGB = Helper.GetRGB(Helper.Blend(sts.HotBackColor3, sts.BackColor, sts.HotBackColor3.A / 255))
                .SMGrayTextRGB = Helper.GetRGB(Helper.Blend(sts.GrayTextColor, sts.BackColor, sts.GrayTextColor.A / 255))
                .SMBackBlendRGB = Helper.GetRGB(Helper.Blend(SM.highLightLineColor, sts.BackColor, SM.highLightLineColor.A / 255))
                .SMWrongTextRGB = Helper.GetRGB(SM.wrongtxClr)
                .SMUBTextRGB = Helper.GetRGB(SM.ubtxClr)
                .SMUETextRGB = Helper.GetRGB(SM.uetxClr)
                .rBackS = sts.BackColor
                .rSelS = sts.HightlightBackColor
            End If
        End With

        Return rb
    End Function
    Friend Function GetRowStyles(ByVal line As Long, ByVal rb As RenderBox) As RenderChar()
        Dim box As BoxItem = rb.Box
        Dim boxIndex As Integer = box.Index
        Dim sourceTrans As ITransformer = SelectedBox.trans
        Dim trans As ITransformer = box.trans
        Dim ftrans As ITransformer = SM.Box(0).trans

        Dim rMap As RenderChar
        Dim rMaps(rb.CharsPerRow - 1) As RenderChar
        For i As Integer = 0 To rMaps.Length - 1
            rMaps(i) = New RenderChar
        Next

        Dim isSel As Boolean
        Dim dataLen As Integer
        Dim isHighlight As Boolean

        Dim SL As SelectionManager = rb.SL
        Dim TI As AdvancedTransformInfo = rb.TI

        Dim A As Integer = line * SM.col_count
        Dim Iy As Integer = 0

        Dim sItem As SelectionItem = rb.SL.Curent
        Dim caret As Long = sItem.car
        If sItem.LF Then caret -= 1

        Dim caretLine As Long = Math.Floor(caret / rb.CharsPerRow)
        Dim index As Long = caretLine * SM.col_count
        isHighlight = A <= index And index < (A + SM.col_count)
        isHighlight = isHighlight And SM.highlightLine

        Dim Idx As Long
        Dim Idy As Long
        Dim isPartial As Boolean
        Dim thePos As Long
        Dim isWrong As Boolean
        Dim isUB As Boolean
        Dim isUE As Boolean
        Dim isHotFind As Boolean

        For i As Integer = 0 To rb.CharsPerRow - 1
            Idx = line * rb.CharsPerRow + i
            rMap = rMaps(i)
            rMap.Font = rb.rFontS

            isSel = SL.IsSelected(Idx) And (A + Iy) < rb.afterLength

            Idy = A + Iy - QR.Position
            isPartial = False
            If Iy < rb.colFloor Then
                dataLen = trans.LengthPerData
            Else
                dataLen = SM.col_count - Iy
                isPartial = True
            End If

            If Idy >= QR.Length Then
                dataLen = 0
            ElseIf Idy < QR.Length And QR.Length < Idy + trans.LengthPerData Then
                dataLen = QR.Length - Idy

                Dim chrLen As Integer = trans.LengthPerData * 2
                If (i Mod rb.perData) >= chrLen Then
                    dataLen = 0
                End If

                If chrLen > trans.CharsPerData Then
                    chrLen = trans.CharsPerData
                    If (i - (Iy * rb.perData)) >= chrLen Then
                        dataLen = 0
                    End If
                End If

                isPartial = True
            End If

            thePos = A + Iy

            If toggleHotFind Or ftoggleHotFind Then
                Dim linep As Long = Math.Floor(thePos / SM.col_count)
                Dim p As Long = thePos Mod SM.col_count
                p = Math.Floor(p / ftrans.LengthPerData) * ftrans.LengthPerData
                p = linep * SM.col_count + p
                isHotFind = hotFind.Contains(p)
            Else
                isHotFind = False
            End If

            isWrong = False
            isUB = ub.IsInRange(A + Iy, dataLen)
            isUE = ue.IsInRange(A + Iy, dataLen)
            If Not isUB And Not isUE Then
                For Each i2 In WF
                    If i2.Position = thePos Then
                        isWrong = True
                        Exit For
                    End If
                Next
            Else
                dataLen = 0
            End If

            If dataLen = trans.LengthPerData Then
            ElseIf dataLen = 0 Then
                rMap.TextColor = rb.SMTextRGB
                If isHighlight Then
                    rMap.BackColor = rb.SMBackBlendRGB
                Else
                    rMap.BackColor = rb.SMBackRGB
                End If
            Else
                rMap.IsPartial = True
                isPartial = True
            End If

            If isHighlight Then
                If isSel Then
                    rMap.TextColor = rb.SMSelTextRGB
                    rMap.BackColor = rb.SMSelBackRGB
                    rb.rSel = rb.rSelS
                Else
                    Select Case True
                        Case isUB
                            rMap.TextColor = rb.SMUBTextRGB
                            rMap.BackColor = rb.SMBackBlendRGB
                        Case isHotFind
                            Dim bkBlend As Color = Helper.Blend(SM.highLightLineColor, SM.bkClr, SM.highLightLineColor.A / 255)
                            rMap.TextColor = rb.SMOvrText3RGB
                            rMap.BackColor = Helper.GetRGB(Helper.Blend(SM.ovrbk3Clr, bkBlend, SM.ovrbk3Clr.A / 255))
                        Case isUE
                            rMap.TextColor = rb.SMUETextRGB
                            rMap.BackColor = rb.SMBackBlendRGB
                        Case isWrong
                            If isPartial And Not dataLen = 0 Then
                                rMap.TextColor = rb.SMWrongTextRGB
                                rMap.BackColor = rb.SMGrayBackRGB
                            Else
                                rMap.TextColor = rb.SMWrongTextRGB
                                rMap.BackColor = rb.SMBackBlendRGB
                            End If
                        Case Else
                            If isPartial And Not dataLen = 0 Then
                                rMap.TextColor = rb.SMGrayTextRGB
                                rMap.BackColor = rb.SMGrayBackRGB
                            Else
                                rMap.TextColor = rb.SMTextRGB
                                rMap.BackColor = rb.SMBackBlendRGB
                            End If
                    End Select
                End If
            Else
                If isSel Then
                    rMap.TextColor = rb.SMSelTextRGB
                    rMap.BackColor = rb.SMSelBackRGB
                    rb.rSel = rb.rSelS
                Else
                    Select Case True
                        Case isUB
                            rMap.TextColor = rb.SMUBTextRGB
                            rMap.BackColor = rb.SMBackRGB
                        Case isHotFind
                            rMap.TextColor = rb.SMOvrText3RGB
                            rMap.BackColor = rb.SMOvrBack3RGB
                        Case isUE
                            rMap.TextColor = rb.SMUETextRGB
                            rMap.BackColor = rb.SMBackRGB
                        Case isWrong
                            If isPartial And Not dataLen = 0 Then
                                rMap.TextColor = rb.SMWrongTextRGB
                                rMap.BackColor = rb.SMGrayBackRGB
                            Else
                                rMap.TextColor = rb.SMWrongTextRGB
                                rMap.BackColor = rb.SMBackRGB
                            End If
                        Case Else
                            If isPartial And Not dataLen = 0 Then
                                rMap.TextColor = rb.SMGrayTextRGB
                                rMap.BackColor = rb.SMGrayBackRGB
                            Else
                                rMap.TextColor = rb.SMTextRGB
                                rMap.BackColor = rb.SMBackRGB
                            End If
                    End Select

                End If
            End If
            rb.rBack = rb.rBackS

            If SL.SelectionLength = 0 And SM.wMode = WriteMode.Overwrite And SM.ovrMode.HasFlag(OverModes.Color) And (A + Iy) < rb.afterLength Then
                If FocussedBoxIndex = boxIndex Then
                    If isPartial Then
                        Dim chrLen As Integer = trans.LengthPerData * 2
                        If SL.IsOver(Idx, rb.TI) And (i Mod rb.perData) < chrLen Then
                            Select Case True
                                Case isUB
                                    rMap.TextColor = rb.SMUBTextRGB
                                    rMap.BackColor = rb.SMOvrBackRGB
                                Case isUE
                                    rMap.TextColor = rb.SMUETextRGB
                                    rMap.BackColor = rb.SMOvrBackRGB
                                Case isWrong
                                    rMap.TextColor = rb.SMWrongTextRGB
                                    rMap.BackColor = rb.SMOvrBackRGB
                                Case Else
                                    rMap.TextColor = rb.SMOvrTextRGB
                                    rMap.BackColor = rb.SMOvrBackRGB
                            End Select
                        End If
                    Else
                        If SL.IsOver(Idx, rb.TI) Then
                            Select Case True
                                Case isUB
                                    rMap.TextColor = rb.SMUBTextRGB
                                    rMap.BackColor = rb.SMOvrBackRGB
                                Case isUE
                                    rMap.TextColor = rb.SMUETextRGB
                                    rMap.BackColor = rb.SMOvrBackRGB
                                Case isWrong
                                    rMap.TextColor = rb.SMWrongTextRGB
                                    rMap.BackColor = rb.SMOvrBackRGB
                                Case Else
                                    rMap.TextColor = rb.SMOvrTextRGB
                                    rMap.BackColor = rb.SMOvrBackRGB
                            End Select
                        End If
                    End If
                Else
                    If isPartial Then
                        Idx = line * TI.CharsPerRow + i
                        Dim chrLen As Integer = trans.LengthPerData * 2
                        If SL.IsOver(Idx, TI) And (i Mod rb.perData) < chrLen Then
                            Select Case True
                                Case isUB
                                    rMap.TextColor = rb.SMUBTextRGB
                                    rMap.BackColor = rb.SMOvrBack2RGB
                                Case isUE
                                    rMap.TextColor = rb.SMUETextRGB
                                    rMap.BackColor = rb.SMOvrBack2RGB
                                Case isWrong
                                    rMap.TextColor = rb.SMWrongTextRGB
                                    rMap.BackColor = rb.SMOvrBack2RGB
                                Case Else
                                    rMap.TextColor = rb.SMOvrText2RGB
                                    rMap.BackColor = rb.SMOvrBack2RGB
                            End Select
                        End If
                    Else
                        Idx = line * TI.CharsPerRow
                        Idx += Math.Floor(Iy / sourceTrans.LengthPerData) * sourceTrans.LengthPerData * (trans.CharsPerData + trans.Sparator)
                        If SL.IsOver(Idx, TI) Then
                            Select Case True
                                Case isUB
                                    rMap.TextColor = rb.SMUBTextRGB
                                    rMap.BackColor = rb.SMOvrBack2RGB
                                Case isUE
                                    rMap.TextColor = rb.SMUETextRGB
                                    rMap.BackColor = rb.SMOvrBack2RGB
                                Case isWrong
                                    rMap.TextColor = rb.SMWrongTextRGB
                                    rMap.BackColor = rb.SMOvrBack2RGB
                                Case Else
                                    rMap.TextColor = rb.SMOvrText2RGB
                                    rMap.BackColor = rb.SMOvrBack2RGB
                            End Select
                        End If
                    End If
                End If
            End If

            For Each ts In SM.TSS
                Dim isContains As Boolean = False
                If ts.StyleTarget = StyleTarget.All Or ts.StyleTarget = StyleTarget.AllContents Or ts.StyleTarget = StyleTarget.SelectedContents Then
                    Dim continuex As Boolean = False
                    If ts.StyleTarget = StyleTarget.SelectedContents Then
                        continuex = ts.BoxIndex.Contains(boxIndex)
                    Else
                        continuex = True
                    End If
                    If continuex Then
                        If ts.Unit = PointUnit.Byte Then
                            If ts.Contains(A + Iy) Then
                                isContains = True
                            End If
                        Else
                            If ts.BoxIndex.Count > 0 Then
                                Dim ixx As Integer = ts.BoxIndex(0)
                                If Not ixx = boxIndex Then
                                    If ts.Contains(Idx, SM.Box(ixx), box) Then
                                        isContains = True
                                    End If
                                Else
                                    If ts.Contains(Idx) Then
                                        isContains = True
                                    End If
                                End If
                            Else
                                If ts.Contains(Idx) Then
                                    isContains = True
                                End If
                            End If
                        End If
                    End If
                End If

                If isContains Then
                    If isSel Then
                        If Not ts.HightlightTextColor.IsEmpty Then
                            rMap.TextColor = Helper.GetRGB(ts.HightlightTextColor)
                        ElseIf ts.IsOverride Then
                            rMap.TextColor = rb.SMSelTextRGB
                        End If
                        If Not ts.HightlightBackColor.IsEmpty Then
                            rMap.BackColor = Helper.GetRGB(ts.HightlightBackColor)
                            rb.rSel = ts.HightlightBackColor
                        ElseIf ts.IsOverride Then
                            rMap.BackColor = rb.SMSelBackRGB
                            rb.rSel = rb.rSelS
                        End If
                    Else
                        Select Case True
                            Case isUB
                                If Not ts.UnAccessableTextColor.IsEmpty Then
                                    rMap.TextColor = Helper.GetRGB(ts.UnAccessableTextColor)
                                Else
                                    rMap.TextColor = rb.SMUBTextRGB
                                End If
                            Case isHotFind
                                If Not ts.HotTextColor3.IsEmpty Then
                                    rMap.TextColor = Helper.GetRGB(ts.HotTextColor3)
                                Else
                                    rMap.TextColor = rb.SMOvrText3RGB
                                End If
                            Case isUE
                                If Not ts.UnEditableTextColor.IsEmpty Then
                                    rMap.TextColor = Helper.GetRGB(ts.UnEditableTextColor)
                                Else
                                    rMap.TextColor = rb.SMUETextRGB
                                End If
                            Case isWrong
                                If Not ts.WrongTextColor.IsEmpty Then
                                    rMap.TextColor = Helper.GetRGB(ts.WrongTextColor)
                                Else
                                    rMap.TextColor = rb.SMWrongTextRGB
                                End If
                            Case Else
                                If Not ts.TextColor.IsEmpty Then
                                    rMap.TextColor = Helper.GetRGB(ts.TextColor)
                                ElseIf ts.IsOverride Then
                                    rMap.TextColor = rb.SMTextRGB
                                End If
                        End Select

                        If isHotFind And Not ts.HotBackColor3.IsEmpty Then
                            If isHighlight Then
                                Dim bkBlend As Color = Helper.Blend(ts.HotBackColor3, rb.rBackS, ts.HotBackColor3.A / 255)
                                rMap.BackColor = Helper.GetRGB(Helper.Blend(SM.highLightLineColor, bkBlend, SM.highLightLineColor.A / 255))
                            Else
                                rMap.BackColor = Helper.GetRGB(Helper.Blend(ts.HotBackColor3, rb.rBackS, ts.HotBackColor3.A / 255))
                            End If
                        ElseIf Not ts.BackColor.IsEmpty Then
                            If isHighlight Then
                                Dim bkBlend As Color = Helper.Blend(ts.BackColor, rb.rBackS, ts.BackColor.A / 255)
                                rMap.BackColor = Helper.GetRGB(Helper.Blend(SM.highLightLineColor, bkBlend, SM.highLightLineColor.A / 255))
                            Else
                                rMap.BackColor = Helper.GetRGB(Helper.Blend(ts.BackColor, rb.rBackS, ts.BackColor.A / 255))
                            End If
                        ElseIf ts.IsOverride Then
                            If isHighlight Then
                                rMap.BackColor = rb.SMBackBlendRGB
                            Else
                                rMap.BackColor = rb.SMBackRGB
                            End If
                        End If
                    End If

                    If Not ts.BackColor.IsEmpty Then
                        rb.rBack = Helper.Blend(ts.BackColor, rb.rBackS, ts.BackColor.A / 255)
                    ElseIf ts.IsOverride Then
                        rb.rBack = rb.rBackS
                    End If

                    If SL.SelectionLength = 0 And SM.wMode = WriteMode.Overwrite And SM.ovrMode.HasFlag(OverModes.Color) And (A + Iy) < rb.afterLength Then
                        If FocussedBoxIndex = boxIndex Then
                            If SL.IsOver(Idx, rb.TI) Then
                                If Not ts.HotTextColor.IsEmpty Then
                                    rMap.TextColor = Helper.GetRGB(ts.HotTextColor)
                                Else
                                    rMap.TextColor = rb.SMOvrTextRGB
                                End If

                                If Not ts.HotBackColor.IsEmpty Then
                                    rMap.BackColor = Helper.GetRGB(Helper.Blend(ts.HotBackColor, rb.rBack, ts.HotBackColor.A / 255))
                                Else
                                    Dim ovrbk As Color = SM.HotBackColor
                                    If Not IsNothing(box.Style) Then
                                        If Not box.Style.HotBackColor.IsEmpty Then
                                            ovrbk = box.Style.HotBackColor
                                        End If
                                    End If
                                    rMap.BackColor = Helper.GetRGB(Helper.Blend(ovrbk, rb.rBack, ovrbk.A / 255))
                                End If
                            End If
                        Else
                            If Me.SL.IsOver(Idx, rb.TI) Then
                                If Not ts.HotTextColor2.IsEmpty Then
                                    rMap.TextColor = Helper.GetRGB(ts.HotTextColor2)
                                Else
                                    rMap.TextColor = rb.SMOvrText2RGB
                                End If
                                If Not ts.HotBackColor2.IsEmpty Then
                                    rMap.BackColor = Helper.GetRGB(Helper.Blend(ts.HotBackColor2, rb.rBack, ts.HotBackColor2.A / 255))
                                Else
                                    Dim ovrbk As Color = SM.HotBackColor2
                                    If Not IsNothing(box.Style) Then
                                        If Not box.Style.HotBackColor2.IsEmpty Then
                                            ovrbk = box.Style.HotBackColor2
                                        End If
                                    End If
                                    rMap.BackColor = Helper.GetRGB(Helper.Blend(ovrbk, rb.rBack, ovrbk.A / 255))
                                End If
                            End If
                        End If
                    End If

                    If Not IsNothing(ts.Font) Then
                        rMap.Font = ts.Font
                    ElseIf ts.IsOverride Then
                        rMap.Font = rb.rFontS
                    End If

                End If
            Next

            If isSel Then
                If Not rb.rSel.IsEmpty Then
                    rMap.BackColor = Helper.GetRGB(Helper.Blend(rb.rSel, rb.rBack, rb.rSel.A / 255))
                End If
            End If

            If (i + 1) Mod rb.perData = 0 And Not i = 0 Or trans.CharsPerData = 1 Then
                Iy += trans.LengthPerData
            End If
        Next

        Return rMaps
    End Function
    Friend Function GetCharsPerRow(box As BoxItem) As Integer
        Dim perData As Integer = box.trans.CharsPerData + box.trans.Sparator
        Dim colFloor As Integer = Math.Floor(SM.col_count / box.trans.LengthPerData) * box.trans.LengthPerData
        Dim charsPerRow As Integer = (colFloor / box.trans.LengthPerData * perData)
        Dim charsLessPerRow As Integer = (colFloor / box.trans.LengthPerData * perData)
        Dim perLess As Integer = (SM.col_count - colFloor) * 2
        'If (SM.col_count - colFloor) > 0 Then perLess += box.trans.Sparator
        charsPerRow += perLess
        Return charsPerRow
    End Function
#End Region

#Region "Indicator"
    Friend indicator_blink As Boolean = True

    Private Sub Timer1_Tick(sender As Object, e As EventArgs) Handles TimerIndicator.Tick
        If Not Me.Focused Then Exit Sub
        indicator_blink = Not indicator_blink
        RefreshIndicator()
    End Sub
    Private Sub TransformBox_GotFocus(sender As Object, e As EventArgs) Handles MyBase.GotFocus
        indicator_blink = True
        TimerIndicator.Stop()
        TimerIndicator.Start()
        RefreshIndicator()
    End Sub
    Private Sub TransformBox_LossFocus(sender As Object, e As EventArgs) Handles MyBase.LostFocus
        indicator_blink = False
        TimerIndicator.Stop()
        RefreshIndicator()
    End Sub

    Private Sub RefreshIndicator()
        Dim A As Long = vscroll_val * SM.col_count
        Dim rect As New Rectangle(SM.all_padLeft, SM.all_padTop, MyRect.Width, MyRect.Height)
        If Not SM.offset_mode = OffsetMode.Hidden Then
            rect.X += SM.offset_width
        End If

        If rect.Height <= 0 Then Exit Sub
        rect.X -= hscroll_val

        Dim fd As FontData = Nothing
        If Not SM.header_mode = HeaderMode.Hidden Then
            rect.Y += SM.header_height + SM.content_padTop
        End If
        If slider_show Then
            rect.Y += slider_size.Height
        End If

        rect.Height = SM.row_height

        Dim bY As Integer = rect.Y

        Dim sourceBox As BoxItem = SM.Box(FocussedBoxIndex)
        Dim sItem2 As SelectionItem = Me.SL.Curent
        Dim caretz As Long = sItem2.car
        If sItem2.LF Then caretz -= 1
        Dim line As Long = Math.Floor(caretz / GetCharsPerRow(sourceBox))
        Dim index As Long = line * SM.col_count
        Dim isHighLight As Boolean = False

        For boxIndex As Integer = 0 To SM.Box.Count - 1
            Dim box As BoxItem = SM.Box(boxIndex)

            Dim N As Long = 0
            Dim Ix As Integer = 0
            Dim Tx As String = ""
            Dim subTx As String = ""
            Dim Idx As Long = 0
            Dim Iy As Integer = 0

            rect.Y = bY

            Dim rb As RenderBox = GetBoxStyles(boxIndex)
            Dim rMaps() As RenderChar
            Dim rGroups As New List(Of RenderGroup)
            Dim rMap As RenderChar = Nothing
            Dim rGroup As RenderGroup = Nothing
            Dim rX As Single = 0
            Dim SL As SelectionManager = rb.SL

            Dim isSel As Boolean = False
            Dim isContains As Boolean = False
            Dim rBack As Color = Nothing
            Dim rSel As Color = Nothing
            Dim found As Boolean = False
            For y As Integer = 0 To ShowedRowCount - 1
                Ix = (A - QR.Position) + y * SM.col_count
                N = (vscroll_val + y) * SM.col_count
                isHighLight = N <= index And index < (N + SM.col_count)
                isHighLight = isHighLight And SM.highlightLine

                rMaps = GetRowStyles(vscroll_val + y, rb)
                Tx = GetRowText(vscroll_val + y, box.trans, True)

                For i As Integer = 0 To rb.CharsPerRow - 1
                    rMap = rMaps(i)
                    If rGroups.Count = 0 Then
                        rGroup = New RenderGroup
                        rGroup.Start = 1
                        rGroup.Length = 1
                        rGroup.Style = rMap
                        rGroups.Add(rGroup)
                    Else
                        If rGroup.Style.TextColor = rMap.TextColor And rGroup.Style.BackColor = rMap.BackColor And rGroup.Style.Font Is rMap.Font Then
                            rGroup.Length += 1
                        Else
                            rGroup = New RenderGroup
                            rGroup.Start = i + 1
                            rGroup.Length = 1
                            rGroup.Style = rMap
                            rGroups.Add(rGroup)
                        End If
                    End If
                Next

                rX = 0

                If boxIndex = 1 Then
                    Dim hh = 4
                End If
                If FocussedBoxIndex = boxIndex And Not (SM.wMode = WriteMode.Overwrite And SL.SelectionLength = 0) Then
                    rX = -1
                    Dim px As Integer = 0
                    Dim pxByt As Byte() = Nothing
                    Dim w As Integer = 0
                    Dim LFx As Integer = 0
                    Dim sItem As SelectionItem = SL.Curent
                    For i As Integer = 0 To rb.CharsPerRow - 1
                        Idx = (A + y * SM.col_count) / SM.col_count * rb.CharsPerRow + i
                        rMap = rMaps(i)
                        fd = GetFontData(rMap.Font)
                        w = Helper.GetTextWidth(fd.ABC, Mid(Tx, i + 1, 1))
                        If SL.IsCaret(Idx) Then
                            If sItem.LF Then
                                LFx = w + 1
                            Else
                                LFx = 0
                            End If
                            Me.Invalidate(New Rectangle(rect.X + box.xpad + rX + LFx, rect.Y, SM.indicatorSize, SM.row_height))
                            Exit For
                        End If

                        rX += w
                    Next
                Else 'If SM.WriteMode = WriteMode.Over And SL.SelectionLength = 0 And (SM.OverMode.HasFlag(OverModes.Line) Or SelectedBoxIndex = boxIndex) Then
                    rX = -1
                    Dim px As Integer = 0
                    Dim pxByt As Byte() = Nothing
                    Dim w As Integer = 0
                    Dim h As Integer = 0
                    Dim IsOvered As Boolean = False
                    For i As Integer = 0 To rb.CharsPerRow - 1
                        Idx = (A + y * SM.col_count) / SM.col_count * rb.CharsPerRow + i
                        rMap = rMaps(i)
                        fd = GetFontData(rMap.Font)
                        w = Helper.GetTextWidth(fd.ABC, Mid(Tx, i + 1, 1))
                        If FocussedBoxIndex = boxIndex Then
                            If SL.IsCaret(Idx) Then
                                h = SM.row_height ' Helper.GetTextHeight(fd.hFont)
                                Me.Invalidate(New Rectangle(rect.X + box.xpad + rX, rect.Y, w, h))
                                found = True
                                Exit For
                            End If
                        Else
                            If Me.SL.IsOver(Idx, rb.TI) Then
                                h = SM.row_height ' Helper.GetTextHeight(fd.hFont)
                                Me.Invalidate(New Rectangle(rect.X + box.xpad + rX, rect.Y, w, h))
                                IsOvered = True
                                found = True
                            ElseIf IsOvered Then
                                Exit For
                            End If
                        End If

                        rX += w
                    Next
                End If

                rect.Y += SM.row_height

                rGroups.Clear()

                If found Then Exit For
            Next
            'End If
            rect.X += box.w
        Next
    End Sub

#End Region

#Region "Mouse Event Handler"
    Friend isDown As Boolean = False
    Friend isDownR As Boolean = False
    Friend m As Point
    Friend isMove As Boolean = False
    Friend LCtrlEnabled As Boolean = False
    Friend dontSnap As Boolean = False
    Friend multisel As Boolean = True
    Friend fullRowSelection As Boolean = False
    Friend dbSnap As Boolean = False
    Friend revCur As Cursor = Helper.FlipedCursor
    Friend curentCur As Integer = 1
    Friend selPower As Boolean = False
    Friend fullseldir As Boolean = False
    Friend enabledrag As Boolean = False
    Friend ondrag As Boolean = False
    Friend ondragp As Point
    Friend DA As New DragAttachment(Me)
    Friend isDroped As Boolean = False
    Friend dragCaret As Long
    Friend dragLF As Boolean
    Friend dragBoxIndex As Integer
    Friend m2 As Point
    Friend dbsnapx As Boolean
    Friend enContextMenu As Boolean = True

    Private Sub TransformBox_MouseDown(sender As Object, e As MouseEventArgs) Handles MyBase.MouseDown
        m = e.Location

        If e.Button = MouseButtons.Left Then
            isDown = True
            isMove = False
            dbSnap = False
            fullseldir = False
            ondrag = False
            dbsnapx = False
            slider_down = False
            slider_hot = False
            If e.Clicks = 2 Then
                dbSnap = True
            End If
            Dim rl As Integer = QR.Update(vscroll_val * SM.col_count, ShowedLength)
            indicator_blink = True
            TimerIndicator.Stop()
            TimerIndicator.Start()
            Me.Focus()
            Me.Select()
            fullRowSelection = False

            If enabledrag And IsInHighlight() Then
                If e.Clicks = 1 Then
                    ondrag = True
                    ondragp = m
                    isDroped = False
                Else
                    dbSnap = False
                    MouseDownEvent(True)
                End If
            Else
                MouseDownEvent(True)
            End If

            If Not dbSnap Then CursorChanger()
            InvalidateCollumnHeader()
            Me.Update()
        ElseIf e.Button = MouseButtons.Right Then
            Me.Select()
            Me.Focus()
            MouseDownEvent(False)
            isDownR = True
        End If
    End Sub
    Private Sub TransformBox_MouseMove(sender As Object, e As MouseEventArgs) Handles MyBase.MouseMove
        m = e.Location

        If isDown Then
            indicator_blink = True
            TimerIndicator.Stop()
            TimerIndicator.Start()
            ScrollMoveEvent()
            Dim rl As Integer = QR.Update(vscroll_val * SM.col_count, ShowedLength)
            dontSnap = False

            If ondrag Then
                If Not m = ondragp And Not isDroped Then
                    ''Dim dd As New DataObject(DataFormats.Text, "A")
                    'DoDragDrop(DA, DragDropEffects.Copy Or DragDropEffects.Move)
                    isDroped = True
                    indicator_blink = False
                End If
            Else
                If SM.AutoSnap Then
                    If Not SL.SelectionLength = 0 And Not isMove Then
                        Dim sItem As SelectionItem = SL.Curent
                        Dim bakItem As SelectionItem = sItem.Clone
                        If sItem.anc < sItem.car Then
                            Dim box As BoxItem = SM.Box(FocussedBoxIndex)
                            Dim charsPerRow As Integer = GetCharsPerRow(box)
                            Dim line As Long = Math.Floor(sItem.anc / charsPerRow)
                            Dim col As Long = sItem.anc Mod charsPerRow
                            Dim perData As Integer = (box.trans.CharsPerData + box.trans.Sparator)
                            Dim colmod As Integer = col Mod perData

                            ' Dim Ix As Long = line * SM.col_count
                            '  Ix += Math.Floor(col / perData) * box.trans.LengthPerData
                            ' Ix -= QR.Position
                            ' Dim data As String = box.trans.GetString(QR.Buffer, Ix)
                            Dim chrLen As Integer = box.trans.CharsPerData 'data.Length

                            Dim isInSparator As Boolean = colmod >= chrLen
                            If isInSparator Then
                                sItem.anc += perData - colmod
                                If sItem.anc > sItem.car Then
                                    sItem.car = sItem.anc
                                End If
                            End If
                        End If

                        SL.SnapSelection(sItem)
                        Dim item As New SelectionItem
                        item.anc = bakItem.car
                        item.car = sItem.car
                        item.LF = bakItem.LF

                        InvalidateSEL(Me.SL, Me.SL.trans, item)
                        InvalidateSEL(SL, SL.trans, sItem)
                        InvalidateSEL(SL, SL.trans, bakItem)
                        isMove = True
                    End If
                End If
                MouseMoveEvent()
            End If

            InvalidateCollumnHeader()
            Me.Update()
        Else
            If Not isDownR Then CursorChanger()

            If slider_show Then
                Dim box As BoxItem = SelectedBox
                Dim w As Integer = box.Width
                Dim rect1 As New Rectangle(SM.all_padLeft, SM.all_padTop, Math.Max(MyRect.Width, GetContentWidth() + extraW + 20), slider_size.Height)
                rect1.X -= hscroll_val
                If Not SM.offset_mode = OffsetMode.Hidden Then
                    rect1.X += SM.offset_width
                End If

                rect1.Width = w
                For Each i In SM.Box
                    If i Is box Then Exit For
                    rect1.X += i.w
                Next
                Dim rect2 As Rectangle = rect1

                rect2.Width = slider_size.Width
                rect2.X += slider_val / 100 * (rect1.Width - rect2.Width)
                If rect2.Contains(m) Then
                    If Not slider_hot Then
                        slider_hot = True
                        Me.InvalidateCollumnHeader()
                    End If
                Else
                    If slider_hot Then
                        slider_hot = False
                        Me.InvalidateCollumnHeader()
                    End If
                End If
            End If

        End If
    End Sub
    Private Sub TransformBox_MouseUp(sender As Object, e As MouseEventArgs) Handles MyBase.MouseUp
        If isDown Then
            If Not SL.SelectionLength = 0 Then
                'dontSnap = False
                Dim rl As Integer = QR.Update(vscroll_val * SM.col_count, ShowedLength)
                If SM.AutoSnap Or LCtrlEnabled Then
                    Dim bakSL As SelectionManager = Me.SL.Clone
                    Dim bakItem As SelectionItem = SL.Curent.Clone
                    If dontSnap Then
                        dontSnap = False
                    Else
                        SL.SnapSelection(SL.Curent)
                    End If

                    Dim item As New SelectionItem
                    item.anc = bakItem.car
                    item.car = bakItem.car
                    item.LF = bakItem.LF
                    If shift_val = SM.col_count Then
                        InvalidateSEL(SL, SL.trans, bakItem)
                    End If
                    CheckSelectionBounds()
                    Dim evt As New HexBoxSelectionEventArgs(FocussedBoxIndex, bakSL, FocussedBoxIndex, Me.SL)
                    RaiseEvent SelectionChanged(Me, evt)
                    If evt.Cancel Then
                        Me.SL = evt.OldSelection
                        InvalidateSEL(evt.OldSelection, SM.Box(FocussedBoxIndex).trans)
                        InvalidateSEL(evt.NewSelection, SM.Box(FocussedBoxIndex).trans)
                    End If

                    InvalidateSEL(SL, SL.trans, item)
                    InvalidateSEL(SL, SL.trans, SL.Curent)
                End If
                InvalidateCollumnHeader()
                Me.Update()
            End If
        End If

        If slider_down Then
            InvalidateCollumnHeader()
        End If

        ondrag = False
        isDown = False
        slider_down = False
        isDownR = False
        dbSnap = False
        fullRowSelection = False
    End Sub
    Friend Sub MouseDownEvent(ByVal left As Boolean)
        If MyRect.Height <= 0 Then Exit Sub

        Dim rect As New Rectangle(SM.all_padLeft, SM.all_padTop, MyRect.Width, SM.row_height)

        rect.X -= hscroll_val

        If Not SM.offset_mode = OffsetMode.Hidden Then
            rect.X += SM.offset_width
        End If
        If Not SM.header_mode = HeaderMode.Hidden Then
            rect.Y += SM.header_height + SM.content_padTop
        End If
        If slider_show Then
            rect.Y += slider_size.Height
        End If

        Dim bY As Integer = rect.Y
        Dim A As Long = vscroll_val * SM.col_count
        Dim ShowedRowCount As Integer = Me.ShowedRowCount
        Dim afterLength As Long = bb.GetLength + shift_val_pre
        Dim remainData As Long = afterLength - A
        Dim remainRow As Long = Math.Ceiling(remainData / SM.col_count)
        Dim containLastRow As Boolean = False

        Dim fullColSelection As Boolean = False
        Dim boxWidth As Integer = 0
        Dim boxHeight As Integer = SM.row_height * ShowedRowCount
        Dim m As Point = Me.m

        If ShowedRowCount >= remainRow Then
            ShowedRowCount = remainRow
            containLastRow = True
        End If

        For Each i In SM.Box
            boxWidth += i.w
        Next

        If Not SM.header_mode = HeaderMode.Hidden Then
            boxHeight += SM.header_height + SM.content_padTop
        End If

        If m.X < rect.X Then
            m.X = rect.X
            fullRowSelection = True
        End If

        If m.X > rect.X + boxWidth Then
            m.X = rect.X + boxWidth
        End If

        If slider_show Then
            Dim rect1 As New Rectangle(SM.all_padLeft, SM.all_padTop, Math.Max(MyRect.Width, GetContentWidth() + extraW + 20), slider_size.Height)
            If m.Y < rect1.Bottom Then
                Dim box As BoxItem = SelectedBox
                Dim w As Integer = box.Width
                rect1.X -= hscroll_val
                If Not SM.offset_mode = OffsetMode.Hidden Then
                    rect1.X += SM.offset_width
                End If

                rect1.Width = w
                For Each i In SM.Box
                    If i Is box Then Exit For
                    rect1.X += i.w
                Next
                Dim rect2 As Rectangle = rect1

                rect2.Width = slider_size.Width
                rect2.X += slider_val / 100 * (rect1.Width - rect2.Width)

                If rect1.Contains(m) Then
                    If left Then
                        slider_down = True
                        If rect2.Contains(m) Then
                            sliderDownX = Math.Abs(rect2.X - m.X)
                        Else
                            sliderDownX = rect2.Width / 2
                        End If
                        If sliderDownX < 0 Then sliderDownX = rect2.Width / 2
                        If sliderDownX > rect2.Width Then sliderDownX = rect2.Width

                        slider_val = ((m.X - sliderDownX) - rect1.X) / (rect1.Width - rect2.Width) * 100
                        If slider_val > 100 Then slider_val = 100
                        If slider_val < 0 Then slider_val = 0

                        shift_val = slider_val / 100 * SM.col_count
                    Else
                        slider_push = Not slider_push
                        cancelContextMenu = True
                    End If
                    Me.Invalidate()
                    Exit Sub
                End If
            End If
        End If

        Dim my As Integer = m.Y
        If m.Y < rect.Y Then
            If Not SM.header_mode = HeaderMode.Hidden Then
                dbSnap = True
            End If
            m.Y = rect.Y
            fullColSelection = True
        End If

        If fullRowSelection And fullColSelection Then
            Dim pdt As Integer = 0
            If Not SM.header_mode = HeaderMode.Hidden Then
                pdt = SM.content_padTop
            End If
            If my < rect.Y - pdt Then
                isMove = True
                SelectAll()
                Exit Sub
            End If
        End If

        If m.Y > boxHeight Then
            m.Y = boxHeight
        End If

        For boxIndex As Integer = 0 To SM.Box.Count - 1
            Dim box As BoxItem = SM.Box(boxIndex)
            rect.Y = bY

            Dim fd As FontData = Nothing
            Dim rb As RenderBox = GetBoxStyles(boxIndex)
            Dim rMaps() As RenderChar
            Dim rMap As RenderChar = Nothing
            Dim Tx As String = ""
            Dim subTx As String = ""
            Dim SL As SelectionManager = rb.SL
            Dim w As Single = 0
            Dim rX As Integer = 0
            Dim r As New Rectangle
            Dim cpr As Integer = 0
            Dim LF As Boolean = False
            Dim line As Long = 0

            For y As Integer = 0 To ShowedRowCount - 1
                line = vscroll_val + y

                rMaps = GetRowStyles(line, rb)
                Tx = GetRowText(line, box.trans, True)

                cpr = rb.CharsPerRow
                If containLastRow Then
                    If y = ShowedRowCount - 1 Then
                        cpr = rb.TI.MaxAllCharsLength Mod rb.TI.CharsPerRow
                        If cpr = 0 Then
                            cpr = rb.TI.CharsPerRow
                        Else
                            cpr += 1
                        End If
                    End If
                End If

                rX = 0
                For i As Integer = 0 To cpr - 1
                    subTx = Mid(Tx, i + 1, 1)
                    fd = GetFontData(rMaps(i).Font)
                    w = Helper.GetTextWidth(fd.ABC, subTx)

                    r.X = rect.X + box.xpad + rX
                    r.Y = rect.Y
                    r.Width = w
                    r.Height = SM.row_height

                    LF = False
                    If i = cpr - 1 Then
                        r.Width = box.w - (rX + w)
                        If m.X > r.X + w Then
                            LF = True
                        End If
                    End If

                    If rX + r.Width > box.w Then
                        r.Width = box.w - rX
                        If r.Width < 0 Then r.Width = 0
                    End If
                    If y = ShowedRowCount - 1 And containLastRow Then
                        r.Height = MyRect.Height
                    End If

                    If i = 0 Then
                        r.X -= box.xpad
                        r.Width += box.xpad
                    End If

                    If r.Contains(m) Then
                        Dim bakIndex As Integer = FocussedBoxIndex
                        Dim bak As SelectionManager = Me.SL.Clone
                        Dim bakTrans As ITransformer = SelectedBox.trans

                        If boxIndex = FocussedBoxIndex Then
                            SL = Me.SL
                        End If

                        Dim anchor As Long = line * rb.TI.CharsPerRow + i
                        Dim caret As Long = anchor
                        If LF Then
                            anchor += 1
                            caret += 1
                        End If

                        If dbSnap Or fullRowSelection Then
                            Dim dLen As Integer = rb.TI.PerData
                            If fullRowSelection Then
                                dLen = rb.TI.CharsPerRow
                            End If

                            Dim index As Integer = Math.Floor(i / rb.TI.PerData) * rb.TI.PerData
                            If index + dLen > cpr Then
                                dLen = cpr - index
                                If dLen < 0 Then dLen = 0
                            End If

                            anchor = line * rb.TI.CharsPerRow + index
                            caret = anchor + dLen

                            If (caret Mod rb.TI.CharsPerRow) = 0 And Not caret = 0 Then
                                LF = True
                            Else
                                LF = False
                            End If
                        End If

                        If caret < 0 Then caret = 0
                        If caret > rb.TI.MaxAllCharsLength Then
                            caret = rb.TI.MaxAllCharsLength
                            If caret Mod rb.TI.CharsPerRow = 0 And Not caret = 0 Then
                                LF = True
                            Else
                                LF = False
                            End If
                        End If

                        If left Then
                            Dim sItem As New SelectionItem
                            sItem.anc = anchor
                            sItem.car = caret
                            sItem.LF = LF
                            sItem.SM = SL

                            If Not LCtrlEnabled Then
                                SL.itms.Clear()
                            End If

                            SL.itms.Add(sItem)
                        End If

                        FocussedBoxIndex = boxIndex
                        Me.SL = SL
                        Dim evt As New HexBoxSelectionEventArgs(bakIndex, bak, boxIndex, SL)
                        RaiseEvent SelectionChanged(Me, evt)
                        If evt.Cancel Then
                            Me.SL = bak
                            FocussedBoxIndex = bakIndex
                        End If
                        Me.InvalidateSelection(bak)

                        For Each ihot In hotFind
                            InvalidateLine(Math.Floor(ihot / SM.col_count))
                        Next
                        InitializeHotFind()
                        For Each ihot In hotFind
                            InvalidateLine(Math.Floor(ihot / SM.col_count))
                        Next
                        Exit Sub
                    End If

                    rX += w
                Next

                rect.Y += SM.row_height
            Next

            rect.X += box.w
        Next
    End Sub
    Friend Sub MouseMoveEvent()
        If MyRect.Height <= 0 Then Exit Sub

        Dim rect As New Rectangle(SM.all_padLeft, SM.all_padTop, MyRect.Width, SM.row_height)

        rect.X -= hscroll_val

        If Not SM.offset_mode = OffsetMode.Hidden Then
            rect.X += SM.offset_width
        End If
        If Not SM.header_mode = HeaderMode.Hidden Then
            rect.Y += SM.header_height + SM.content_padTop
        End If
        If slider_show Then
            rect.Y += slider_size.Height
        End If

        Dim bY As Integer = rect.Y
        Dim A As Long = vscroll_val * SM.col_count
        Dim ShowedRowCount As Integer = Me.ShowedRowCount
        Dim afterLength As Long = bb.GetLength + shift_val_pre
        Dim remainData As Long = afterLength - A
        Dim remainRow As Long = Math.Ceiling(remainData / SM.col_count)
        Dim containLastRow As Boolean = False

        Dim boxWidth As Integer = 0
        Dim boxHeight As Integer = SM.row_height * (ShowedRowCount - 1)
        Dim m As Point = Me.m

        If ShowedRowCount >= remainRow Then
            ShowedRowCount = remainRow
            containLastRow = True
        End If

        For Each i In SM.Box
            boxWidth += i.w
        Next

        If Not SM.header_mode = HeaderMode.Hidden Then
            boxHeight += SM.header_height + SM.content_padTop
        End If

        If m.X < rect.X Then
            m.X = rect.X
        End If

        If m.X > rect.X + boxWidth Then
            m.X = rect.X + boxWidth
        End If

        If m.Y < rect.Y Then
            m.Y = rect.Y
        End If

        If m.Y > boxHeight Then
            m.Y = boxHeight
        End If

        If slider_down Then
            Dim box As BoxItem = SelectedBox
            Dim w As Integer = box.Width
            Dim rect1 As New Rectangle(SM.all_padLeft, SM.all_padTop, Math.Max(MyRect.Width, GetContentWidth() + extraW + 20), slider_size.Height)
            rect1.X -= hscroll_val
            If Not SM.offset_mode = OffsetMode.Hidden Then
                rect1.X += SM.offset_width
            End If

            rect1.Width = w
            For Each i In SM.Box
                If i Is box Then Exit For
                rect1.X += i.w
            Next
            Dim rect2 As Rectangle = rect1

            rect2.Width = slider_size.Width

            slider_val = ((m.X - sliderDownX) - rect1.X) / (rect1.Width - rect2.Width) * 100
            If slider_val > 100 Then slider_val = 100
            If slider_val < 0 Then slider_val = 0

            shift_val = slider_val / 100 * SM.col_count
            CheckSelectionBounds()
            Me.Invalidate()
            Exit Sub
        End If

        For boxIndex As Integer = 0 To SM.Box.Count - 1
            Dim box As BoxItem = SM.Box(boxIndex)
            rect.Y = bY

            Dim fd As FontData = Nothing
            Dim rb As RenderBox = GetBoxStyles(boxIndex)
            Dim rMaps() As RenderChar
            Dim rMap As RenderChar = Nothing
            Dim Tx As String = ""
            Dim subTx As String = ""
            Dim SL As SelectionManager = rb.SL
            Dim w As Single = 0
            Dim rX As Integer = 0
            Dim r As New Rectangle
            Dim cpr As Integer = 0
            Dim LF As Boolean = False
            Dim line As Long = 0

            For y As Integer = 0 To ShowedRowCount - 1
                line = vscroll_val + y

                rMaps = GetRowStyles(line, rb)
                Tx = GetRowText(line, box.trans, True)

                cpr = rb.CharsPerRow
                If containLastRow Then
                    If y = ShowedRowCount - 1 Then
                        cpr = rb.TI.MaxAllCharsLength Mod rb.TI.CharsPerRow
                        If cpr = 0 Then
                            cpr = rb.TI.CharsPerRow
                        Else
                            cpr += 1
                        End If
                    End If
                End If

                rX = 0
                For i As Integer = 0 To cpr - 1
                    subTx = Mid(Tx, i + 1, 1)
                    fd = GetFontData(rMaps(i).Font)
                    w = Helper.GetTextWidth(fd.ABC, subTx)

                    r.X = rect.X + box.xpad + rX
                    r.Y = rect.Y
                    r.Width = w
                    r.Height = SM.row_height

                    LF = False
                    If i = cpr - 1 Then
                        r.Width = box.w - rX - box.xpad
                        If m.X > r.X + w Then
                            LF = True
                        End If
                    End If

                    If rX + box.xpad + r.Width > box.w Then
                        r.Width = box.w - rX - box.xpad
                        If r.Width < 0 Then r.Width = 0
                    End If
                    If y = ShowedRowCount - 1 And containLastRow Then
                        r.Height = MyRect.Height
                        'If LF Then LF = False
                    End If

                    If i = 0 Then
                        r.X -= box.xpad
                        r.Width += box.xpad
                    End If

                    If r.Contains(m) Then
                        Dim bakIndex As Integer = FocussedBoxIndex
                        Dim bak As SelectionManager = Me.SL.Clone
                        Dim bakTrans As ITransformer = Me.SelectedBox.trans
                        Dim bakItem As SelectionItem = SL.Curent.Clone

                        Dim anchor As Long = line * rb.TI.CharsPerRow + i
                        Dim caret As Long = anchor
                        If LF Then
                            anchor += 1
                            caret += 1
                        End If

                        If dbSnap Or fullRowSelection Then
                            Dim dLen As Integer = rb.TI.PerData
                            If fullRowSelection Then
                                dLen = rb.TI.CharsPerRow
                            End If
                            Dim index As Integer = Math.Floor(i / rb.TI.PerData) * rb.TI.PerData
                            If index + dLen >= cpr Then
                                dLen = cpr - index
                                If dLen < 0 Then dLen = 0
                            End If
                            If i = 0 And m.X < (rect.X + box.xpad + rX) And Not fullRowSelection And dbSnap Then dLen = 0
                            anchor = line * rb.TI.CharsPerRow + index
                            caret = anchor + dLen

                            If (caret Mod rb.TI.CharsPerRow) = 0 And Not caret = 0 And Not dLen = 0 Then
                                LF = True
                            Else
                                LF = False
                            End If
                        End If

                        Dim sItem As SelectionItem = SL.Curent
                        If fullRowSelection Then
                            If sItem.anc >= caret Then
                                If Not fullseldir Then
                                    fullseldir = True
                                    sItem.anc = caret + rb.TI.CharsPerRow
                                End If
                                caret -= rb.TI.CharsPerRow
                                LF = False
                            Else
                                If fullseldir Then
                                    fullseldir = False
                                    caret -= rb.TI.CharsPerRow
                                    sItem.anc = caret - rb.TI.CharsPerRow
                                End If
                                LF = True
                            End If
                        ElseIf dbSnap Then
                            If sItem.anc >= caret Then
                                Dim pd As Integer = 0

                                Dim v As Integer = (sItem.anc Mod rb.TI.CharsPerRow) / rb.TI.PerData
                                If v < rb.TI.DataCount Or rb.TI.PartialCount = 0 Then
                                    pd = rb.TI.PerData
                                Else
                                    pd = rb.TI.PartialCount * 2
                                End If
                                If Not dbsnapx Then
                                    dbsnapx = True
                                    Dim oldAncLine As Long = Math.Floor(sItem.anc / rb.TI.CharsPerRow)
                                    sItem.anc = sItem.anc + pd

                                    Dim newLine As Long = Math.Floor(sItem.anc / rb.TI.CharsPerRow)
                                    If Not oldAncLine = newLine Then
                                        sItem.anc = oldAncLine * rb.TI.CharsPerRow + rb.TI.CharsPerRow
                                    End If
                                End If

                                v = caret Mod rb.TI.CharsPerRow
                                If v < rb.TI.DataCount * rb.TI.PerData And Not rb.TI.PartialCount = 0 Then
                                    pd = rb.TI.PartialCount * 2
                                Else
                                    pd = rb.TI.PerData
                                End If
                                If Not i = cpr - 1 Then
                                    caret -= pd
                                    LF = False
                                End If

                                Dim newLine2 As Long = Math.Floor(caret / rb.TI.CharsPerRow)
                                If Not line = newLine2 Then
                                    caret = line * rb.TI.CharsPerRow
                                End If

                            Else
                                If dbsnapx Then
                                    dbsnapx = False
                                    Dim pd As Integer = 0

                                    Dim v As Integer = sItem.anc Mod rb.TI.CharsPerRow
                                    If v <= rb.TI.DataCount * rb.TI.PerData And Not rb.TI.PartialCount = 0 Then
                                        pd = rb.TI.PartialCount * 2
                                    Else
                                        pd = rb.TI.PerData
                                    End If

                                    caret -= pd

                                    v = (sItem.anc Mod rb.TI.CharsPerRow) / rb.TI.PerData
                                    If Not v = 0 Or rb.TI.PartialCount = 0 Then
                                        pd = rb.TI.PerData
                                    Else
                                        pd = rb.TI.PartialCount * 2
                                    End If
                                    'Me.Parent.Text = sItem.anc Mod rb.TI.CharsPerRow

                                    Dim oldAncLine As Long = Math.Floor(sItem.anc / rb.TI.CharsPerRow)
                                    sItem.anc = sItem.anc - pd

                                    Dim newLine As Long = Math.Floor(caret / rb.TI.CharsPerRow)
                                    If Not line = newLine Then
                                        caret = line * rb.TI.CharsPerRow + rb.TI.CharsPerRow
                                        LF = True
                                    End If

                                    newLine = Math.Floor(sItem.anc / rb.TI.CharsPerRow)
                                    If Not oldAncLine = newLine And y = ShowedRowCount - 1 And containLastRow Then
                                        sItem.anc = oldAncLine * rb.TI.CharsPerRow
                                    End If
                                End If
                            End If
                        End If

                        If caret < 0 Then caret = 0
                        If caret > rb.TI.MaxAllCharsLength Then
                            caret = rb.TI.MaxAllCharsLength
                            If caret Mod rb.TI.CharsPerRow = 0 And Not caret = 0 Then
                                LF = True
                            Else
                                LF = False
                            End If
                        End If
                        If sItem.anc > rb.TI.MaxAllCharsLength Then sItem.anc = rb.TI.MaxAllCharsLength

                        If Not isMove And SM.AutoSnap Then
                            'isMove = True
                            If Not dbSnap And Not fullRowSelection Then
                                ' SL.SnapSelection(sItem, SnapEffect.Anchor)

                            End If
                        End If

                        Dim evt As New HexBoxSelectionEventArgs(bakIndex, bak, boxIndex, SL)
                        RaiseEvent SelectionChanged(Me, evt)
                        If Not evt.Cancel Then
                            sItem.car = caret
                            sItem.LF = LF
                            sItem.SM = SL
                            If Not boxIndex = FocussedBoxIndex Then
                                FocussedBoxIndex = boxIndex
                                Me.SL = SL
                                Me.InvalidateSelection(bak)
                            Else
                                Dim difItem As New SelectionItem(SL, bakItem.car, sItem.car, sItem.LF)
                                If bakItem.LF Then
                                    difItem.anc -= 1
                                End If
                                Me.InvalidateSEL(SL, box.trans, difItem)

                                Me.InvalidateSELAC(SL, bakItem)
                                Me.InvalidateSELAC(SL, sItem)
                            End If
                        End If

                        For Each ihot In hotFind
                            InvalidateLine(Math.Floor(ihot / SM.col_count))
                        Next
                        InitializeHotFind()

                        Exit Sub
                    End If

                    rX += w
                Next

                rect.Y += SM.row_height
            Next

            rect.X += box.w
        Next
    End Sub
    Friend Sub ScrollMoveEvent()
        If slider_show And slider_down Then Exit Sub
        Dim vscroll As Integer = 0
        Dim hscroll As Integer = 0

        Dim maxLength As Long = Math.Ceiling((bb.GetLength + shift_val_pre) / SM.col_count)
        If maxLength >= ShowedRowCount Then
            If m.Y > MyRect.Height Then
                vscroll = 1
            ElseIf m.Y < 0 Then
                vscroll = -1
            End If
        End If

        Dim wContent As Integer = GetContentWidth()
        If MyRect.Width < wContent Then

            If m.X > MyRect.Width Then
                hscroll = wContent
            ElseIf m.X < 0 Then
                hscroll = -wContent
            End If
        End If

        If Not vscroll = 0 Then
            VScrollBar_SetValue(VScrollBar_GetValue() + vscroll)
            vscroll = VScrollBar_GetValue()
            If vscroll_enhanced Then
                vscroll = vscroll / vscroll_max_scroll * vscroll_max_value
            End If
            If Not vscroll_val = vscroll Then
                vscroll_val = vscroll
                ShowedStart = vscroll_val * SM.col_count
                Me.Invalidate(New Rectangle(0, 0, GetContentWidth, MyRect.Height))
            End If
        End If
        If Not hscroll = 0 Then
            HScrollBar_SetValue(HScrollBar_GetValue() + hscroll)
            hscroll = HScrollBar_GetValue()
            If Not hscroll_val = hscroll Then
                hscroll_val = hscroll
                Me.Invalidate()
            End If
        End If
    End Sub
    Friend Sub CursorChanger()
        Dim A As Long = vscroll_val * SM.col_count
        Dim rect As New Rectangle(SM.all_padLeft, SM.all_padTop, MyRect.Width, MyRect.Height)

        If rect.Height <= 0 Then Exit Sub
        rect.X -= hscroll_val

        Dim cur As Integer = 1 '0=default '1=ibeam '2=reverse
        If IsInHighlight() Then
            cur = 0
        End If
        Dim hh As Integer = 0

        If slider_show Then
            hh = slider_size.Height
        End If
        If Not SM.header_mode = HeaderMode.Hidden Then
            If m.Y <= SM.header_height + hh Then
                cur = 0
            End If
        End If

        If slider_show Then
            Dim box As BoxItem = SelectedBox
            Dim w As Integer = box.Width
            Dim rect1 As New Rectangle(SM.all_padLeft, SM.all_padTop, Math.Max(MyRect.Width, GetContentWidth() + extraW + 20), slider_size.Height)
            rect1.X -= hscroll_val
            If Not SM.offset_mode = OffsetMode.Hidden Then
                rect1.X += SM.offset_width
            End If

            rect1.Width = w
            For Each i In SM.Box
                If i Is box Then Exit For
                rect1.X += i.w
            Next
            Dim rect2 As Rectangle = rect1

            rect2.Width = slider_size.Width
            rect2.X += slider_val / 100 * (rect1.Width - rect2.Width)
            If rect2.Contains(m) Then
                cur = 0
            End If
        End If

        If Not SM.offset_mode = OffsetMode.Hidden Then
            If m.X < rect.X + SM.offset_width Then
                cur = 2
            End If
        End If
        If Not cur = Me.curentCur Then
            Me.curentCur = cur
            Select Case cur
                Case 0
                    Me.Cursor = Cursors.Arrow
                Case 1
                    Me.Cursor = Cursors.IBeam
                Case 2
                    Me.Cursor = revCur
            End Select
        End If
    End Sub
    Friend Function IsInHighlight() As Boolean
        Dim A As Long = vscroll_val * SM.col_count
        Dim rect As New Rectangle(SM.all_padLeft, SM.all_padTop, MyRect.Width, MyRect.Height)
        If Not SM.offset_mode = OffsetMode.Hidden Then
            rect.X += SM.offset_width
        End If

        If rect.Height <= 0 Then Return False
        rect.X -= hscroll_val

        Dim fd As FontData = Nothing
        If Not SM.header_mode = HeaderMode.Hidden Then
            rect.Y += SM.header_height + SM.content_padTop
        End If
        If slider_show Then
            rect.Y += slider_size.Height
        End If

        rect.Height = SM.row_height

        Dim bY As Integer = rect.Y

        Dim ShowedRowCount As Integer = Me.ShowedRowCount
        Dim remainData As Long = bb.GetLength + shift_val_pre - A
        Dim remainRow As Long = Math.Ceiling(remainData / SM.col_count)
        Dim LastRowLength As Integer = SM.col_count
        If ShowedRowCount >= remainRow Then
            ShowedRowCount = remainRow
            LastRowLength = remainData Mod SM.col_count
            If LastRowLength = 0 Then LastRowLength = SM.col_count
        End If

        Dim boxHeight As Integer = SM.row_height * ShowedRowCount

        If Not SM.header_mode = HeaderMode.Hidden Then
            boxHeight += SM.header_height + SM.content_padTop
        End If

        Dim sourceBox As BoxItem = SM.Box(FocussedBoxIndex)
        Dim sItem2 As SelectionItem = Me.SL.Curent
        Dim caretz As Long = sItem2.car
        If sItem2.LF Then caretz -= 1
        Dim line As Long = Math.Floor(caretz / GetCharsPerRow(sourceBox))
        Dim index As Long = line * SM.col_count
        Dim isHighLight As Boolean = False

        QR.Update(vscroll_val * SM.col_count, ShowedLength)

        For boxIndex As Integer = 0 To SM.Box.Count - 1
            Dim box As BoxItem = SM.Box(boxIndex)

            Dim N As Long = 0
            Dim Ix As Integer = 0
            Dim Tx As String = ""
            Dim subTx As String = ""
            Dim Idx As Long = 0
            Dim Iy As Integer = 0

            rect.Y = bY

            Dim rb As RenderBox = GetBoxStyles(boxIndex)
            Dim rMaps() As RenderChar
            Dim rGroups As New List(Of RenderGroup)
            Dim rMap As RenderChar = Nothing
            Dim rGroup As RenderGroup = Nothing
            Dim rX As Single = 0
            Dim SL As SelectionManager = rb.SL

            Dim isSel As Boolean = False
            Dim isContains As Boolean = False

            For y As Integer = 0 To ShowedRowCount - 1
                Ix = (A - QR.Position) + y * SM.col_count
                N = (vscroll_val + y) * SM.col_count
                isHighLight = N <= index And index < (N + SM.col_count)
                isHighLight = isHighLight And SM.highlightLine

                rMaps = GetRowStyles(vscroll_val + y, rb)
                Tx = GetRowText(vscroll_val + y, box.trans, True)

                Dim rectX As New Rectangle
                rX = 0
                Dim w As Integer = 0

                For i As Integer = 0 To rb.CharsPerRow - 1
                    rMap = rMaps(i)
                    fd = GetFontData(rMap.Font)
                    subTx = Mid(Tx, i + 1, 1)
                    w = Helper.GetTextWidth(fd.ABC, subTx)
                    rectX.X = rect.X + box.xpad + rX
                    rectX.Y = rect.Y
                    rectX.Width = w
                    rectX.Height = SM.row_height

                    If rectX.Contains(m) Then
                        line = (vscroll_val + y) * rb.CharsPerRow
                        Dim colIndex As Integer = i
                        Dim absIndex As Long = line + colIndex
                        If SL.IsSelected(absIndex) Then
                            Return True
                        Else
                            Return False
                        End If
                    End If
                    rX += w
                Next

                rect.Y += SM.row_height

                rGroups.Clear()
            Next
            rect.X += box.w
        Next
        Return False
    End Function

    Private Sub HexBox_DragEnter(sender As Object, e As DragEventArgs) Handles MyBase.DragEnter
        e.Effect = DragDropEffects.Move

        If e.Data.GetDataPresent(DataFormats.StringFormat) Then
            e.Effect = DragDropEffects.Move
        ElseIf e.Data.GetDataPresent(DataFormats.Text) Then
            e.Effect = DragDropEffects.Move
        ElseIf e.Data.GetDataPresent(DataFormats.UnicodeText) Then
            e.Effect = DragDropEffects.Move
        ElseIf e.Data.GetDataPresent(DragAttachment.myFormat) Then
            e.Effect = DragDropEffects.Move
        End If

    End Sub

    Private Sub HexBox_DragDrop(sender As Object, e As DragEventArgs) Handles MyBase.DragDrop
        If Not isDroped Then
            TransformBox_MouseUp(sender, New MouseEventArgs(MouseButtons.None, 1, m.X, m.Y, 0))
            InvalidateSEL(Me.SL, SelectedBox.trans, New SelectionItem(Me.SL, dragCaret, dragCaret, dragLF))
        End If
    End Sub

    Private Sub HexBox_DragOver(sender As Object, e As DragEventArgs) Handles MyBase.DragOver
        If Not isDroped Then
            m2 = Me.PointToClient(New Point(e.X, e.Y))
            Dim allowDrop As Boolean = DragOverEvent()

            If allowDrop Then
                e.Effect = e.AllowedEffect
            Else
                e.Effect = DragDropEffects.None
            End If
            ' Me.Parent.Text = "[" & dragBoxIndex & "] " & dragCaret
        End If
    End Sub

    Friend Function DragOverEvent() As Boolean
        Dim rect As New Rectangle(SM.all_padLeft, SM.all_padTop, MyRect.Width, MyRect.Height)
        If Not SM.offset_mode = OffsetMode.Hidden Then
            rect.X += SM.offset_width
        End If

        If rect.Height <= 0 Then Return False
        rect.X -= hscroll_val

        If Not SM.header_mode = HeaderMode.Hidden Then
            rect.Y += SM.header_height + SM.content_padTop
        End If

        rect.Height = SM.row_height

        Dim bY As Integer = rect.Y

        Dim A As Long = vscroll_val * SM.col_count
        Dim ShowedRowCount As Integer = Me.ShowedRowCount
        Dim remainData As Long = bb.GetLength + shift_val_pre - A
        Dim remainRow As Long = Math.Ceiling(remainData / SM.col_count)
        Dim LastRowLength As Integer = SM.col_count
        If ShowedRowCount >= remainRow Then
            ShowedRowCount = remainRow
            LastRowLength = remainData Mod SM.col_count
            If LastRowLength = 0 Then LastRowLength = SM.col_count
        End If

        Dim boxWidth As Integer = 0
        For Each i In SM.Box
            boxWidth += i.w
        Next

        Dim boxHeight As Integer = SM.row_height * ShowedRowCount
        If Not SM.header_mode = HeaderMode.Hidden Then
            boxHeight += SM.header_height + SM.content_padTop
        End If

        Dim m As Point = Me.m2
        If m.X < rect.X Then
            m.X = rect.X
        End If

        If m.X > rect.X + boxWidth Then
            m.X = rect.X + boxWidth
        End If

        If m.Y < rect.Y Then
            m.Y = rect.Y
        End If

        If m.Y > boxHeight Then
            m.Y = boxHeight
        End If

        For boxIndex As Integer = 0 To SM.Box.Count - 1
            Dim box As BoxItem = SM.Box(boxIndex)

            rect.Y = bY

            Dim fd As FontData = Nothing
            Dim Ix As Integer = 0
            Dim Tx As String = ""
            Dim subTx As String = ""
            Dim Idx As Long = 0
            Dim Iy As Integer = 0
            Dim rb As RenderBox = GetBoxStyles(boxIndex)
            Dim rMaps() As RenderChar
            Dim rGroups As New List(Of RenderGroup)
            Dim rMap As RenderChar = Nothing
            Dim rGroup As RenderGroup = Nothing
            Dim rX As Single = 0
            Dim isSel As Boolean = False
            Dim isContains As Boolean = False
            Dim SL As SelectionManager = rb.SL

            For y As Integer = 0 To ShowedRowCount - 1
                Ix = (A - QR.Position) + y * SM.col_count

                rMaps = GetRowStyles(vscroll_val + y, rb)
                Tx = GetRowText(vscroll_val + y, box.trans, True)

                Dim rectX As New Rectangle
                rX = 0
                Dim w As Integer = 0
                Dim wOrg As Integer = 0
                Dim rectXOrg As Integer = 0
                Dim charsPerRow2 As Integer = rb.CharsPerRow
                Dim LastRowChars As Integer = rb.CharsPerRow
                If y = ShowedRowCount - 1 Then
                    LastRowChars = LastRowLength Mod box.trans.LengthPerData
                    If LastRowChars = 0 Then
                        LastRowChars = LastRowLength / box.trans.LengthPerData * (box.trans.CharsPerData + box.trans.Sparator)
                        charsPerRow2 = LastRowChars
                    Else
                        charsPerRow2 = Math.Floor(LastRowLength / box.trans.LengthPerData) * (box.trans.CharsPerData + box.trans.Sparator) + box.trans.LengthPerData * 2
                        If charsPerRow2 > rb.CharsPerRow Then
                            charsPerRow2 = rb.CharsPerRow
                        End If
                    End If
                End If

                For i As Integer = 0 To charsPerRow2 - 1
                    rMap = rMaps(i)
                    fd = GetFontData(rMap.Font)
                    subTx = Mid(Tx, i + 1, 1)
                    w = Helper.GetTextWidth(fd.ABC, subTx)
                    wOrg = w
                    rectXOrg = rect.X + rX
                    If i = charsPerRow2 - 1 Then
                        w = box.w - rX
                        If SM.Box.Count - 1 = boxIndex Then
                            w = MyRect.Width
                        End If
                    ElseIf i = 0 Then
                        rX -= box.xpad
                        w += box.xpad
                        If boxIndex = 0 Then
                            rX -= rect.X
                            w += rect.X
                        End If
                    End If
                    rectX.X = rect.X + box.xpad + rX
                    rectX.Y = rect.Y
                    rectX.Width = w
                    rectX.Height = SM.row_height

                    If rectX.Contains(m) Then
                        Dim bakIndex As Integer = FocussedBoxIndex
                        Dim line As Long = vscroll_val + y
                        Dim lineIndex As Long = line * GetCharsPerRow(box)
                        Dim colIndex As Integer = i
                        Dim absIndex As Long = lineIndex + colIndex
                        Dim dragLF As Boolean = False
                        If colIndex = charsPerRow2 - 1 And m.X >= rectX.X + wOrg Then
                            dragLF = True
                        Else
                            dragLF = False
                        End If

                        Dim sItem As New SelectionItem
                        sItem.anc = absIndex
                        sItem.car = absIndex
                        sItem.LF = Me.dragLF

                        InvalidateSEL(SL, box.trans, sItem)

                        sItem = New SelectionItem
                        sItem.anc = dragCaret
                        sItem.car = dragCaret
                        sItem.LF = dragLF

                        dragCaret = absIndex
                        dragBoxIndex = boxIndex
                        Me.dragLF = dragLF

                        InvalidateSEL(SL, box.trans, sItem)
                        For Each itm In SL.Items
                            Dim smin As Long = itm.anc + 1
                            Dim smax As Long = itm.car
                            If smin > smax Then
                                smin = itm.car
                                smax = itm.anc
                            End If
                            If smin <= dragCaret And dragCaret < smax Then
                                Return False
                            End If
                        Next

                        Return True
                    End If
                    rX += w
                Next

                rect.Y += SM.row_height

                rGroups.Clear()
            Next
            rect.X += box.w
        Next
        Return False
    End Function

    Friend Function GetMenu(ByVal text As String) As MenuItem
        For Each i In menuItems
            If i.Text = text Then
                Return i
            End If
        Next
        Return Nothing
    End Function
    Friend Function GetMenu(ByVal id As Integer) As MenuItem
        Return GetMenu(menuItems, id)
    End Function
    Friend Function GetMenu(list As List(Of MenuItem), ByVal id As Integer) As MenuItem
        For Each i In list
            If i.ID = id Then
                Return i
            Else
                Dim m As MenuItem = GetMenu(i.Items, id)
                If Not IsNothing(m) Then Return m
            End If
        Next
        Return Nothing
    End Function

    Friend Sub AddSubMenu(ByVal hParentMenu As IntPtr, ByVal menu As MenuItem)
        Dim ix As Integer = 100
        For Each i In menu.Items
            Dim hMenu As IntPtr = USER32.CreateMenu()
            Dim mii As New USER32.MENUITEMINFO
            mii.cbSize = Marshal.SizeOf(mii)
            mii.fMask = i.Flag
            If Not i.Items.Count = 0 Then
                mii.fMask = mii.fMask Or USER32.MenuMasks.MIIM_SUBMENU
            End If
            If Not i.Enabled Then
                mii.fMask = mii.fMask Or USER32.MenuStates.MFS_DISABLED
                mii.fState = USER32.MenuStates.MFS_DISABLED
            End If
            mii.fMask = mii.fMask Or USER32.MenuMasks.MIIM_ID

            mii.hSubMenu = hMenu
            If i.Separator Then
                mii.fType = USER32.MenuMasks.MFT_SEPARATOR
            Else
                mii.fType = USER32.MenuMasks.MFT_STRING
            End If
            i.ID = menu.ID + ix
            mii.dwTypeData = i.Text
            mii.wID = i.ID

            For Each i2 In i.Items
                AddSubMenu(hMenu, i2)
            Next

            USER32.InsertMenuItem(hParentMenu, 1000, True, mii)
            ix += 1
        Next
    End Sub
    Friend Sub ShowContextMenu()
        Dim hWnd As IntPtr = Me.Handle
        Dim hPopupMenu As IntPtr = USER32.CreatePopupMenu()

        GetMenu("&Expand....").Enabled = Me.CanExpand
        GetMenu("&Undo").Enabled = Me.CanUndo
        GetMenu("&Redo").Enabled = Me.CanRedo
        GetMenu("Cu&t").Enabled = Me.CanCut
        GetMenu("&Copy").Enabled = Me.CanCopy
        GetMenu("Advanced Copy").Enabled = Me.CanCopy
        GetMenu("&Paste").Enabled = Me.CanPaste
        GetMenu("&Delete").Enabled = Me.CanDelete
        GetMenu("Select &All").Enabled = Me.CanSelectAll

        Dim ix As Integer = 100
        For Each i In menuItems
            Dim hMenu As IntPtr = USER32.CreateMenu()
            Dim mii As New USER32.MENUITEMINFO
            mii.cbSize = Marshal.SizeOf(mii)
            mii.fMask = i.Flag
            If Not i.Items.Count = 0 Then
                mii.fMask = mii.fMask Or USER32.MenuMasks.MIIM_SUBMENU
            End If
            If Not i.Enabled Then
                mii.fMask = mii.fMask Or USER32.MenuStates.MFS_DISABLED
                mii.fState = USER32.MenuStates.MFS_DISABLED
            End If
            mii.fMask = mii.fMask Or USER32.MenuMasks.MIIM_ID

            If i.Separator Then
                mii.fType = USER32.MenuMasks.MFT_SEPARATOR
            Else
                mii.fType = USER32.MenuMasks.MFT_STRING
            End If

            i.ID = ix
            mii.hSubMenu = hMenu
            mii.dwTypeData = i.Text
            mii.wID = i.ID

            AddSubMenu(hMenu, i)

            USER32.InsertMenuItem(hPopupMenu, 1000, True, mii)
            ix += 1
        Next

        USER32.SetForegroundWindow(hWnd)
        Dim menuId As Integer = USER32.TrackPopupMenuEx(hPopupMenu, USER32.TrackPopupFlag.TPM_TOPALIGN Or USER32.TrackPopupFlag.TPM_LEFTALIGN Or USER32.TrackPopupFlag.TPM_RETURNCMD, MousePosition.X, MousePosition.Y, hWnd, 0)
        If menuId = 0 Then Exit Sub

        Dim selectedMenu As MenuItem = GetMenu(menuId)

        Select Case selectedMenu.Text
            Case "&Expand...."
                Expand()
            Case "&Undo"
                Undo()
            Case "&Redo"
                Redo()
            Case "Cu&t"
                Cut()
            Case "&Goto...."
                If EnableGotoDialog Then
                    If IsNothing(gt) Then
                        gt = New GotoDialog
                    ElseIf gt.frm.IsDisposed Then
                        gt = New GotoDialog
                    End If

                    timer2_Tick(Nothing, Nothing)
                End If
            Case "&Copy"
                Copy()
            Case "&Paste"
                Paste()
            Case "&Delete"
                Delete()
            Case "Select &All"
                SelectAll()
            Case "Current Box Content"
                Dim oldMode As CopyModes = Me.CopyMode
                Me.CopyMode = CopyModes.SelectedContent
                Copy()
                Me.CopyMode = oldMode
            Case "Current Box Content + Offset"
                Dim oldMode As CopyModes = Me.CopyMode
                Me.CopyMode = CopyModes.SelectedContent Or CopyModes.WithOffset
                Copy()
                Me.CopyMode = oldMode
            Case "All Box Content"
                Dim oldMode As CopyModes = Me.CopyMode
                Me.CopyMode = CopyModes.AllContents
                Copy()
                Me.CopyMode = oldMode
            Case "All Box Content + Offset"
                Dim oldMode As CopyModes = Me.CopyMode
                Me.CopyMode = CopyModes.AllContentsWithOffset
                Copy()
                Me.CopyMode = oldMode
            Case "Full Copy"
                Dim oldMode As CopyModes = Me.CopyMode
                Me.CopyMode = CopyModes.Full
                Copy()
                Me.CopyMode = oldMode
            Case "ANSI"
                Dim oldMode As CopyModes = Me.CopyMode
                Me.CopyMode = CopyModes.ANSICopy
                Copy()
                Me.CopyMode = oldMode
            Case "Unicode"
                Dim oldMode As CopyModes = Me.CopyMode
                Me.CopyMode = CopyModes.UnicodeCopy
                Copy()
                Me.CopyMode = oldMode
            Case "Array of Bytes"
                Dim oldMode As CopyModes = Me.CopyMode
                Me.CopyMode = CopyModes.ArrayOfBytesCopy
                Copy()
                Me.CopyMode = oldMode
        End Select

    End Sub
#End Region

#Region "Key Event Handler"
    Dim snapped As Boolean = False
    Dim gt As GotoDialog
    Friend WithEvents timer2 As New Timer
    Public Property EnableGotoDialog As Boolean = True

    Private Sub timer2_Tick(sender As Object, e As EventArgs) Handles timer2.Tick
        timer2.Stop()

        gt.Collumn = SM.col_count
        Dim caret As Long = Me.SL.Curent.Caret
        Dim TI As AdvancedTransformInfo = Transformers.GetAdvancedTransformInfo(Me.SL.trans, SM.col_count, bb.GetLength + shift_val_pre)
        Dim Line As Long = Math.Floor(caret / TI.CharsPerRow)
        Dim Col As Integer = caret Mod TI.CharsPerRow
        If Col = 0 Then
            Line += 1
        Else
            Col = Math.Floor(Col / TI.PerData)
        End If

        If gt.ValueMode = ValueModes.Offset Then
            gt.Value = Line * SM.col_count
        Else
            gt.Value = Line
        End If

        If gt.ShowDialog() = DialogResult.OK Then
            If gt.ValueMode = ValueModes.Offset Then
                Line = gt.Value / SM.col_count
            Else
                Line = gt.Value
            End If
            Me.GoTo(Line)
        End If
    End Sub
    Private Sub TransformBox_KeyDown(sender As Object, e As KeyEventArgs) Handles MyBase.KeyDown
        If e.Control Then
            If Not snapped Then
                snapped = True
                SnapSelection()
            End If
            LCtrlEnabled = True

        End If
        If e.Shift Then
            If Not toggleHotFind And dtoggleHotFind Then
                toggleHotFind = True
                InitializeHotFind()
                For Each ihot In hotFind
                    InvalidateLine(Math.Floor(ihot / SM.col_count))
                Next
            End If
        End If

        If e.Control Then
            e.SuppressKeyPress = True
            e.Handled = True
            If e.KeyCode = Keys.C Then
                Copy()
            ElseIf e.KeyCode = Keys.X Then
                Cut()
            ElseIf e.KeyCode = Keys.Z Then
                If Not CanUndo And CanRedo And bb.um.Items.Length = 2 Then
                    Redo()
                Else
                    Undo()
                End If
            ElseIf e.KeyCode = Keys.Y Then
                Redo()
            ElseIf e.KeyCode = Keys.v Then
                Paste()
            ElseIf e.KeyCode = Keys.A Then
                SelectAll()
            ElseIf e.KeyCode = Keys.G Then
                If EnableGotoDialog Then
                    If IsNothing(gt) Then
                        gt = New GotoDialog
                    ElseIf gt.frm.IsDisposed Then
                        gt = New GotoDialog
                    End If

                    timer2.Start()
                End If
            End If
        ElseIf e.Modifiers = 0 Then
            If e.KeyCode = Keys.Back Then
                If Me.SL.SelectionLength = 0 Then
                    SendKey(Keys.Back, "")
                Else
                    If SM.wMode = WriteMode.Insert Then
                        Delete()
                    Else
                        Clear()
                    End If
                End If
                e.SuppressKeyPress = True
                e.Handled = True
            ElseIf e.KeyCode = Keys.Delete Then
                If SL.SelectionLength = 0 Then
                    SendKey(Keys.Delete, "")
                Else
                    Delete()
                End If
                e.SuppressKeyPress = True
                e.Handled = True
            End If
        End If
    End Sub
    Friend Function ToPosition(ByVal chrPos As Long, ByVal trans As ITransformer) As Long
        Dim TI As AdvancedTransformInfo = Transformers.GetAdvancedTransformInfo(trans, SM.col_count, bb.GetLength + shift_val_pre)
        Dim Line As Long = Math.Floor(chrPos / TI.CharsPerRow)
        Dim Col As Integer = chrPos Mod TI.CharsPerRow
        If Col = 0 Then
            Line += 1
        Else
            Col = Math.Floor(Col / TI.PerData)
        End If
        Return Line * SM.col_count + Col
    End Function
    Friend Function ToPosition(ByVal chrPos As Long, ByVal TI As AdvancedTransformInfo) As Long
        Dim Line As Long = Math.Floor(chrPos / TI.CharsPerRow)
        Dim Col As Integer = chrPos Mod TI.CharsPerRow
        Col = Math.Floor(Col / TI.PerData)
        Return Line * SM.col_count + Col * TI.Trans.LengthPerData
    End Function
    Friend Function ToCharPosition(ByVal pos As Long, ByVal trans As ITransformer) As Long
        Dim TI As AdvancedTransformInfo = Transformers.GetAdvancedTransformInfo(trans, SM.col_count, bb.GetLength + shift_val_pre)
        Dim Line As Long = Math.Floor(pos / SM.col_count)
        Dim Col As Integer = pos Mod SM.col_count
        If Col = 0 Then
            Line += 1
        Else
            Col = Math.Floor(Col / trans.LengthPerData) * TI.PerData
        End If
        Return Line * TI.CharsPerRow + Col
    End Function

    Private Sub TransformBox_KeyPress(sender As Object, e As KeyPressEventArgs) Handles MyBase.KeyPress
        If Not Me.SL.SelectionLength = 0 Then
            If SM.wMode = WriteMode.Insert Then Delete()
        End If
        SendKey(0, e.KeyChar)
    End Sub
    Friend Sub CheckSelectionBounds()
        Dim box As BoxItem = SelectedBox
        Dim trans As ITransformer = box.trans
        Dim TI = Transformers.GetAdvancedTransformInfo(trans, SM.col_count, bb.GetLength + shift_val_pre)
        Dim di As Long = Math.Floor(shift_val_pre / trans.LengthPerData) * TI.PerData
        If di < 0 Then di = 0
        Dim dl As Long = TI.MaxAllCharsLength

        For Each i In SL.Items
            If i.anc < di Then i.anc = di
            If i.anc > dl Then i.anc = dl
            If i.car < di Then i.car = di
            If i.car > dl Then i.car = dl
        Next
    End Sub
    Private Sub SendKey(ByVal key As Keys, c As Char)
        Dim box As BoxItem = SelectedBox
        Dim trans As ITransformer = box.trans
        Dim cpr As Integer = GetCharsPerRow(box)
        Dim perData As Integer = box.trans.CharsPerData + box.trans.Sparator
        Dim caret As Long = SL.Curent.anc
        Dim line As Long = Math.Floor(caret / cpr)
        Dim col As Integer = caret Mod cpr
        Dim rowText As String = GetRowText(line, box.trans, False)
        Dim colMod As Integer = Math.Floor(col / perData) * perData
        Dim car_sub As Integer = col - colMod
        Dim dataText As String = Mid(rowText, colMod + 1, box.trans.CharsPerData)
        Dim newDataText As String = ""
        Dim newDataByte As Byte() = Nothing
        Dim posByte As Long = line * SM.col_count + Math.Floor(col / perData) * trans.LengthPerData

        Dim lchr As New List(Of Char)
        lchr.AddRange(dataText)

        Dim isWrong As Boolean = False
        Dim sWrong As WrongFormatted = Nothing

        For Each i In WF
            If i.Position = posByte And i.Box Is box Then
                isWrong = True
                sWrong = i
                Exit For
            End If
        Next

        Dim ins As Boolean = False

        If key = Keys.Back Then
            Try
                If car_sub - 1 < 0 Then
                    lchr.RemoveAt(0)
                Else
                    lchr.RemoveAt(car_sub - 1)
                End If
            Catch ex As Exception
            End Try
        ElseIf key = Keys.Delete Then
            Try
                If car_sub < lchr.Count Then
                    lchr.RemoveAt(car_sub)
                End If
            Catch ex As Exception
            End Try
        Else
            If SM.wMode = WriteMode.Insert Then
                If trans.Sparator = 0 Then
                    If lchr.Count <= car_sub + 1 Then
                        lchr.Clear()
                        lchr.Add(c)
                    Else
                    End If
                Else
                        If lchr.Count <= car_sub Then
                        If car_sub < trans.CharsPerData Then
                            lchr.Add(c)
                        Else
                            lchr.Clear()
                            lchr.Add(c)
                            ins = True
                        End If
                    Else
                        lchr.Insert(car_sub, c)
                    End If
                End If
            Else
                If lchr.Count <= car_sub Then
                    lchr.Add(c)
                Else
                    lchr(car_sub) = c
                End If
            End If
        End If

        If lchr.Count = 0 And SM.wMode = WriteMode.Insert Then
            Try
                Me.SL.Clear()
                If key = Keys.Back Then
                    bb.Remove(posByte - 1, trans.LengthPerData)
                    Me.SL.Curent.anc = line * cpr + colMod - 1
                ElseIf key = Keys.Delete Then
                    bb.Remove(posByte, trans.LengthPerData)
                    Me.SL.Curent.anc = line * cpr + colMod
                End If

                If Me.SL.Curent.anc < 0 Then Me.SL.Curent.anc = 0
                Me.SL.Curent.car = Me.SL.Curent.anc
                CheckSelectionBounds()
                Me.InvalidateLine(line - vscroll_val, ShowedRowCount)

            Catch ex As Exception

            End Try
        Else
            newDataText = Mid(lchr.ToArray, 1, trans.CharsPerData)

            Try
                newDataByte = trans.GetBytes(newDataText)
            Catch ex As Exception
            End Try

            If lchr.Count = 0 Then
                If SM.wMode = WriteMode.Overwrite Then
                    ReDim newDataByte(trans.LengthPerData - 1)
                End If
            End If

            Me.SL.Clear()
            If IsNothing(newDataByte) Then
                If ins Then
                    ReDim newDataByte(trans.LengthPerData - 1)
                    bb.Insert(posByte + 1, newDataByte)
                    AddWrongFormatted(box, posByte + 1, newDataText)
                    Me.SL.Curent.anc = line * cpr + col + 2
                Else
                    AddWrongFormatted(box, posByte, newDataText)
                    If key = Keys.Back Then
                        Me.SL.Curent.anc = line * cpr + col - 1
                    Else
                        If car_sub = trans.CharsPerData - 1 And SM.wMode = WriteMode.Overwrite Then
                            Me.SL.Curent.anc = line * cpr + col + trans.Sparator + 1
                        Else
                            Me.SL.Curent.anc = line * cpr + col + 1
                        End If
                    End If
                End If
            Else
                If ins Then
                    bb.Insert(posByte + 1, newDataByte)
                    Me.SL.Curent.anc = line * cpr + col + 2
                ElseIf SM.wMode = WriteMode.Insert Then
                    bb.Insert(posByte, newDataByte)
                    Me.SL.Curent.anc = line * cpr + col + 1
                Else
                    bb.Overwrite(posByte, newDataByte)
                    If key = Keys.Back Then
                        If car_sub = 0 Then
                            Me.SL.Curent.anc = line * cpr + col - trans.Sparator - 1
                        Else
                            Me.SL.Curent.anc = line * cpr + col - 1
                        End If
                    Else
                        If car_sub = trans.CharsPerData - 1 And SM.wMode = WriteMode.Overwrite Then
                            Me.SL.Curent.anc = line * cpr + col + trans.Sparator + 1
                        Else
                            Me.SL.Curent.anc = line * cpr + col + 1
                        End If

                    End If
                End If
                If isWrong Then
                    WF.Remove(sWrong)
                End If
            End If
            If Me.SL.Curent.anc < 0 Then Me.SL.Curent.anc = 0
            Me.SL.Curent.car = Me.SL.Curent.anc
            CheckSelectionBounds()
            Me.InvalidateLine(line, 1)
        End If

    End Sub
    Private Sub TransformBox_KeyUp(sender As Object, e As KeyEventArgs) Handles MyBase.KeyUp
        LCtrlEnabled = False
        snapped = False
        If toggleHotFind Then
            toggleHotFind = False
            For Each ihot In hotFind
                InvalidateLine(Math.Floor(ihot / SM.col_count))
            Next
            InitializeHotFind()
        End If
    End Sub

#End Region

#Region "Invalidator"
    Private Sub TransformBox_Resize(sender As Object, e As EventArgs) Handles MyBase.Resize
        RefreshInfo()
    End Sub
    Friend Sub RefreshInfo()
        RefreshInfoWithoutInvalidate()
        Me.Invalidate(MyRect)
        Me.Update()
    End Sub
    Friend Sub RefreshInfoWithoutInvalidate()
        MyRect = Me.ClientRectangle
        If MyRect.Width < 1 Then MyRect.Width = 1
        If MyRect.Height < 1 Then MyRect.Height = 1

        Dim show_str As Long = vscroll_val * SM.col_count
        Dim hh As Integer = 0
        If Not SM.header_mode = HeaderMode.Hidden Then hh = SM.header_height + SM.content_padTop
        Dim showed_row_count As Long = Math.Floor((MyRect.Height - hh) / SM.row_height) + 1
        If showed_row_count < 1 Then showed_row_count = 1
        Dim showed_count As Long = showed_row_count * SM.col_count
        Me.ShowedStart = show_str
        Me.ShowedRowCount = showed_row_count
        Me.ShowedLength = showed_count
        'vscroll
        RefreshVScrollBar()
        Dim vNow As Long = VScrollBar_GetValue()
        Dim pg As Long = vscroll_max_scroll - ShowedRowCount + 1
        If vNow >= pg Then
            If vscroll_enhanced Then
                vscroll_val = VScrollBar_GetValue() / vscroll_max_scroll * vscroll_max_value
            Else
                vscroll_val = VScrollBar_GetValue()
            End If
        End If

        'hscroll
        RefreshHScrollBar()
        hscroll_val = HScrollBar_GetValue()
    End Sub

    Public Sub InvalidateSelection()
        InvalidateSEL(Me.SL, SM.Box(FocussedBoxIndex).trans)
    End Sub
    Public Sub InvalidateSelection(ByVal oldSelection As SelectionManager)
        InvalidateSEL(oldSelection, oldSelection.trans)
        InvalidateSEL(Me.SL, SM.Box(FocussedBoxIndex).trans)
    End Sub
    Public Sub InvalidateSelectionItem(ByVal Item As SelectionItem)
        InvalidateSEL(SL, SL.trans, {Item})
    End Sub
    Public Sub InvalidateSelectionItem(ByVal Item As SelectionItem())
        InvalidateSEL(SL, SL.trans, Item)
    End Sub
    Public Sub InvalidateBytes(ByVal position As Long, ByVal length As Long)
        InvalidatePOS(position, length)
    End Sub
    Friend Sub InvalidateSEL(ByVal sl As SelectionManager, ByVal trans As ITransformer, ByVal Items As SelectionItem())
        Dim A As Long = vscroll_val * SM.col_count
        Dim wContent As Integer = GetContentWidth()
        Dim rect As New Rectangle(SM.all_padLeft, SM.all_padTop, wContent, MyRect.Height)

        If rect.Height <= 0 Then Exit Sub
        rect.X -= hscroll_val

        If Not SM.header_mode = HeaderMode.Hidden Then
            rect.Y += SM.header_height + SM.content_padTop
        End If
        If slider_show Then
            rect.Y += slider_size.Height
        End If

        rect.Height = SM.row_height

        Dim boxHeight As Integer = SM.row_height * (ShowedRowCount - 1)

        Dim perData As Integer = trans.CharsPerData + trans.Sparator
        Dim colFloor As Integer = Math.Floor(SM.col_count / trans.LengthPerData) * trans.LengthPerData
        Dim charsPerRow As Integer = (colFloor / trans.LengthPerData * perData)
        Dim charsLessPerRow As Integer = (colFloor / trans.LengthPerData * perData)
        Dim perLess As Integer = (SM.col_count - colFloor) * 2
        'If (SM.col_count - colFloor) > 0 Then perLess += box.trans.Sparator
        charsPerRow += perLess

        Dim index As Long = 0
        For y As Integer = 0 To ShowedRowCount - 1
            Dim invalidated As Boolean = False
            For Each i In Items
                For ix As Integer = 0 To charsPerRow - 1
                    index = (y + vscroll_val) * charsPerRow + ix
                    If sl.IsSelected(i, index) Then
                        Me.Invalidate(rect)
                        invalidated = True
                        Exit For
                    End If
                Next
                If Not invalidated Then
                    index = (y + vscroll_val) * charsPerRow
                    If i.LF Then
                        If index <= (i.anc - 1) And (i.anc - 1) < index + charsPerRow Then
                            Me.Invalidate(rect)
                        ElseIf index <= (i.car - 1) And (i.car - 1) < index + charsPerRow Then
                            Me.Invalidate(rect)
                        End If
                    Else
                        If index <= i.anc And i.anc < index + charsPerRow Then
                            Me.Invalidate(rect)
                        ElseIf index <= i.car And i.car < index + charsPerRow Then
                            Me.Invalidate(rect)
                        End If
                    End If
                End If
            Next

            rect.Y += SM.row_height
        Next
    End Sub
    Friend Sub InvalidateSEL(ByVal sl As SelectionManager, ByVal trans As ITransformer, ByVal Item As SelectionItem)
        InvalidateSEL(sl, trans, {Item})
    End Sub
    Friend Sub InvalidateSEL(ByVal sl As SelectionManager, ByVal trans As ITransformer)
        InvalidateSEL(sl, trans, sl.Items.ToArray)
    End Sub
    Friend Sub InvalidateSELAC(ByVal sl As SelectionManager, ByVal Item As SelectionItem)
        Dim A As Long = vscroll_val * SM.col_count
        Dim wContent As Integer = GetContentWidth()
        Dim rect As New Rectangle(SM.all_padLeft, SM.all_padTop, wContent, MyRect.Height)

        If rect.Height <= 0 Then Exit Sub
        rect.X -= hscroll_val

        If Not SM.header_mode = HeaderMode.Hidden Then
            rect.Y += SM.header_height + SM.content_padTop
        End If
        If slider_show Then
            rect.Y += slider_size.Height
        End If

        rect.Height = SM.row_height

        Dim boxHeight As Integer = SM.row_height * (ShowedRowCount - 1)

        Dim trans As ITransformer = sl.trans
        Dim perData As Integer = trans.CharsPerData + trans.Sparator
        Dim colFloor As Integer = Math.Floor(SM.col_count / trans.LengthPerData) * trans.LengthPerData
        Dim charsPerRow As Integer = (colFloor / trans.LengthPerData * perData)
        Dim charsLessPerRow As Integer = (colFloor / trans.LengthPerData * perData)
        Dim perLess As Integer = (SM.col_count - colFloor) * 2
        charsPerRow += perLess

        Dim index As Long = 0
        Dim car As Long = Item.car
        If Item.LF Then
            car -= 1
        End If
        Dim c As Integer = 0
        For y As Integer = 0 To ShowedRowCount - 1
            index = (y + vscroll_val) * charsPerRow
            If index <= Item.anc And Item.anc < index + charsPerRow Then
                Me.Invalidate(rect)
                If c = 2 Then Exit For
                c = 1
            ElseIf index <= car And car < index + charsPerRow Then
                Me.Invalidate(rect)
                If c = 1 Then Exit For
                c = 2
            End If
            rect.Y += SM.row_height
        Next
    End Sub
    Friend Sub InvalidateCollumnHeader()
        If SM.header_mode = HeaderMode.Hidden And Not slider_show Then Exit Sub
        Dim hh As Integer = 0
        If slider_show Then
            hh = slider_size.Height
        End If
        Dim rect As New Rectangle(SM.all_padLeft, SM.all_padTop, GetContentWidth, SM.header_height + SM.header_padTop + hh)
        Me.Invalidate(rect)
    End Sub
    Public Sub InvalidateLine(ByVal line As Long, ByVal count As Long)
        Dim lineStart As Long = line
        Dim lineEnd As Long = line + count

        Dim showLineStart As Long = vscroll_val
        Dim showLineEnd As Long = vscroll_val + ShowedRowCount

        Dim isIntersect As Boolean = Helper.IntersectsWith(lineStart, lineEnd, showLineStart, showLineEnd)
        If isIntersect Then
            Dim invalidLineStart As Long = lineStart
            If invalidLineStart < showLineStart Then
                invalidLineStart = showLineStart
            End If

            Dim invalidLineEnd As Long = lineEnd
            If invalidLineEnd > showLineEnd Then
                invalidLineEnd = showLineEnd
            End If

            Dim wContent As Integer = GetContentWidth()
            Dim rect As New Rectangle(SM.all_padLeft, SM.all_padTop, wContent, SM.row_height)

            If rect.Height <= 0 Then Exit Sub
            rect.X -= hscroll_val

            If Not SM.header_mode = HeaderMode.Hidden Then
                rect.Y += SM.header_height + SM.content_padTop
            End If
            If slider_show Then
                rect.Y += slider_size.Height
            End If

            invalidLineStart = invalidLineStart - showLineStart
            invalidLineEnd = invalidLineEnd - showLineStart

            rect.Y += SM.row_height * invalidLineStart
            For y As Integer = invalidLineStart To invalidLineEnd
                Me.Invalidate(rect)
                rect.Y += SM.row_height
            Next
        End If
    End Sub
    Public Sub InvalidateLine(ByVal line As Long)
        Dim showLineStart As Long = vscroll_val
        Dim showLineEnd As Long = vscroll_val + ShowedRowCount
        If Not (showLineStart <= line And line < showLineEnd) Then
            Exit Sub
        End If

        Dim wContent As Integer = GetContentWidth()
        Dim rect As New Rectangle(SM.all_padLeft, SM.all_padTop, wContent, MyRect.Height)

        If rect.Height <= 0 Then Exit Sub
        rect.X -= hscroll_val

        If Not SM.header_mode = HeaderMode.Hidden Then
            rect.Y += SM.header_height + SM.content_padTop
        End If
        If slider_show Then
            rect.Y += slider_size.Height
        End If

        rect.Height = SM.row_height

        Dim invalidLineStart As Long = line - vscroll_val

        rect.Y += SM.row_height * invalidLineStart
        Me.Invalidate(rect)
    End Sub
    Friend Sub InvalidatePOS(ByVal position As Long, ByVal length As Long)
        Dim lineStart As Long = Math.Floor(position / SM.col_count)
        Dim lineEnd As Long = Math.Floor((position + length) / SM.col_count)

        Dim showLineStart As Long = vscroll_val
        Dim showLineEnd As Long = vscroll_val + ShowedRowCount

        Dim isIntersect As Boolean = Helper.IntersectsWith(lineStart, lineEnd, showLineStart, showLineEnd)
        If isIntersect Then
            Dim invalidLineStart As Long = lineStart
            If invalidLineStart < showLineStart Then
                invalidLineStart = showLineStart
            End If

            Dim invalidLineEnd As Long = lineEnd
            If invalidLineEnd > showLineEnd Then
                invalidLineEnd = showLineEnd
            End If

            Dim wContent As Integer = GetContentWidth()
            Dim rect As New Rectangle(SM.all_padLeft, SM.all_padTop, wContent, SM.row_height)

            If rect.Height <= 0 Then Exit Sub
            rect.X -= hscroll_val

            If Not SM.header_mode = HeaderMode.Hidden Then
                rect.Y += SM.header_height + SM.content_padTop
            End If
            If slider_show Then
                rect.Y += slider_size.Height
            End If

            invalidLineStart = invalidLineStart - showLineStart
            invalidLineEnd = invalidLineEnd - showLineStart

            rect.Y += SM.row_height * invalidLineStart
            If (position + length) Mod SM.col_count = 0 Then
                invalidLineEnd -= 1
            End If
            For y As Integer = invalidLineStart To invalidLineEnd
                Me.Invalidate(rect)
                rect.Y += SM.row_height
            Next
        End If
    End Sub

    Friend Sub InvalidateStyle(ByVal ts As TextStyle)
        If ts.StyleTarget = StyleTarget.CollumnHeader Then
        Else
            If ts.Unit = PointUnit.Byte Then
                InvalidatePOS(ts.Position, ts.Length)
            Else
                Dim cpr As Integer = 0
                If ts.StyleTarget = StyleTarget.SelectedContents Then
                    cpr = GetCharsPerRow(SM.Box(ts.BoxIndex(0)))
                Else
                    cpr = GetCharsPerRow(SM.Box(FocussedBoxIndex))
                End If
                Dim lineStart As Long = Math.Floor(ts.Position / cpr)
                Dim lineEnd As Long = Math.Ceiling((ts.Position + ts.Length) / cpr)
                InvalidateLine(lineStart, lineEnd - lineStart)
            End If
        End If
    End Sub
#End Region

#Region "Public"
    <Browsable(False)>
    Public Property CopyMode As CopyModes = CopyModes.SelectedContent
    <Browsable(False)>
    Public ReadOnly Property ByteBuilder As SalSal.ByteBuilder
        Get
            Return bb
        End Get
    End Property
    Public Property ShowInvalidateArea As Boolean
        Get
            Return DrawInvalidateArea
        End Get
        Set(value As Boolean)
            If Not value = DrawInvalidateArea Then
                DrawInvalidateArea = value
                Me.Invalidate()
            End If
        End Set
    End Property
    <Browsable(False)>
    Public ReadOnly Property Selection As SelectionManager
        Get
            Return Me.SL
        End Get
    End Property
    Public Property AutoSnap As Boolean
        Get
            Return SM.AutoSnap
        End Get
        Set(value As Boolean)
            SM.AutoSnap = value
            If value Then
                SnapSelection()
            End If
        End Set
    End Property
    Public Sub SnapSelection()
        Dim trans As ITransformer = Me.SL.trans
        Dim bak As SelectionManager = Me.SL
        Dim snapped As SelectionManager = Me.SL.CreateTransform(trans)

        Me.SL = snapped
        InvalidateSEL(bak, trans)
        InvalidateSEL(snapped, trans)
    End Sub
    Public Property ShowBorder As Boolean
        Get
            Return SM.draw_border
        End Get
        Set(value As Boolean)
            If Not SM.draw_border = value Then
                SM.draw_border = value
                Me.Invalidate()
                Me.Update()
            End If
        End Set
    End Property
    Public Property CollumnHeaderMode As HeaderMode
        Get
            Return SM.header_mode
        End Get
        Set(value As HeaderMode)
            If Not SM.header_mode = value Then
                SM.header_mode = value
                RefreshInfo()
                Me.Invalidate()
                Me.Update()
            End If
        End Set
    End Property
    Public Property OffsetType As OffsetMode
        Get
            Return SM.offset_mode
        End Get
        Set(value As OffsetMode)
            If Not SM.offset_mode = value Then
                SM.offset_mode = value
                SM.UpdateStyle()
                Me.RefreshInfoWithoutInvalidate()
                Me.Invalidate()
                Me.Update()
            End If
        End Set
    End Property
    Public Property HighlightCurrentLine As Boolean
        Get
            Return SM.highlightLine
        End Get
        Set(value As Boolean)
            If Not SM.highlightLine = value Then
                SM.highlightLine = value
                Me.Invalidate()
                Me.Update()
            End If
        End Set
    End Property
    Public Property ViewMode As TransformMode
        Get
            Return SM.Box(0).Mode
        End Get
        Set(value As TransformMode)
            If Not SM.Box(0).Mode = value Then
                SM.Box(0).Mode = value
                SM.UpdateBox(SM.Box(0))
                RefreshInfo()
                Me.Invalidate()
                Me.Update()
            End If
        End Set
    End Property
    Public Property Collumn As Integer
        Get
            Return SM.col_count
        End Get
        Set(value As Integer)
            If Not SM.col_count = value Then
                SM.col_count = value
                SM.UpdateBoxes()
                RefreshInfo()
                Me.Invalidate()
                Me.Update()
            End If
        End Set
    End Property
    Public Property MultiSelection As Boolean
        Get
            Return multisel
        End Get
        Set(value As Boolean)
            If Not multisel = value Then
                multisel = value
                If Not multisel Then
                    If Me.SL.itms.Count > 1 Then
                        Dim curent As SelectionItem = Me.SL.Curent
                        Me.SL.itms.Clear()
                        Me.SL.itms.Add(curent)
                        Me.Invalidate()
                        Me.Update()
                    End If
                End If
            End If
        End Set
    End Property
    Public Sub ScrollLine(ByVal line As Long)
        If vscroll_enhanced Then
            line = line / vscroll_max_scroll
        End If

        VScrollBar_SetValue(line)
        Dim v As Long = VScrollBar_GetValue()
        If vscroll_enhanced Then
            v = v / vscroll_max_scroll * vscroll_max_value
        End If
        If Not vscroll_val = v Then
            vscroll_val = v
            ShowedStart = vscroll_val * SM.col_count
            Dim rect As New Rectangle(0, 0, GetContentWidth, MyRect.Height)
            Me.Invalidate(rect)
            Me.Update()
        End If
    End Sub
    Public Sub ShowFirstLine(ByVal line As Long)
        Dim maxLine As Long = Math.Floor((bb.GetLength + shift_val_pre) / SM.col_count) - ShowedRowCount
        If maxLine < 0 Then maxLine = 0
        If line > maxLine Then line = maxLine
        ScrollLine(line)
    End Sub
    Public Sub EnsureVisibleSelection()
        Dim sItem As SelectionItem = SL.Curent
        EnsureVisibleSelection(sItem.SelectionStart, sItem.SelectionLength)
    End Sub
    Public Sub EnsureVisibleSelection(ByVal position As Long, ByVal length As Long)
        Dim box As BoxItem = SM.Box(FocussedBoxIndex)
        Dim trans As ITransformer = box.trans
        Dim cpr As Integer = GetCharsPerRow(box)

        Dim line As Long = Math.Floor(position / cpr)
        Dim lineEnd As Long = Math.Ceiling((position + length) / cpr)
        Dim lineCount As Long = lineEnd - line
        If lineCount > ShowedRowCount Then
            line = lineEnd - ShowedRowCount
            lineCount = ShowedRowCount
        End If

        EnsureVisible(line, lineCount)
    End Sub
    Public Sub EnsureVisible(ByVal line As Long)
        EnsureVisible(line, 1)
    End Sub
    Public Sub EnsureVisible(ByVal line As Long, ByVal count As Long)
        If vscroll_val <= line And (line + count) <= (vscroll_val + ShowedRowCount) Then
            Exit Sub
        End If

        If count > ShowedRowCount Then count = ShowedRowCount

        If vscroll_val < line Then
            line = vscroll_val + ((line + count) - vscroll_val) - ShowedRowCount
        End If

        ScrollLine(line)
    End Sub
    Public Sub [GoTo](ByVal line As Long)
        SL.Clear()
        SL.AddSelectionByIndex(line * SM.col_count, 0)

        If vscroll_val <= line And line <= (vscroll_val + ShowedRowCount) Then
            EnsureVisible(line)
        Else
            ShowFirstLine(line)
        End If
    End Sub
    Public Property SelectedBoxIndex As Integer
        Get
            Return FocussedBoxIndex
        End Get
        Set(value As Integer)
            If value >= SM.Box.Count - 1 Then
                value = SM.Box.Count - 1
            End If
            If value < 0 Then value = 0
            If Not FocussedBoxIndex = value Then
                Dim OldBoxIndex As Integer = FocussedBoxIndex
                Dim NewBoxIndex As Integer = value
                Dim OldSelection As SelectionManager = Me.SL
                Dim NewSelection As SelectionManager = Me.SL.TransformSelection(SM.Box(FocussedBoxIndex).trans, SM.Box(value).trans)
                Me.SL = NewSelection
                FocussedBoxIndex = value
                Dim evt As New HexBoxSelectionEventArgs(OldBoxIndex, OldSelection, NewBoxIndex, NewSelection)
                RaiseEvent SelectionChanged(Me, evt)

                If evt.Cancel Then
                    Me.SL = evt.OldSelection
                End If

                Me.Invalidate()
            End If
        End Set
    End Property
    Public Property SelectedTransfomer As ITransformer
        Get
            Return SelectedBox.trans
        End Get
        Set(value As ITransformer)
            SelectedBox.Transformer = value
        End Set
    End Property
    <Browsable(False)>
    Public Property SelectedBox As BoxItem
        Get
            Return SM.Box(FocussedBoxIndex)
        End Get
        Set(value As BoxItem)
            Dim index As Integer = SM.Box.IndexOf(value)
            If Not FocussedBoxIndex = index And Not index = -1 Then
                FocussedBoxIndex = index
                Me.Invalidate()
            End If
        End Set
    End Property
    <Browsable(False)>
    Public ReadOnly Property BoxItems As BoxItem()
        Get
            Return SM.Box.ToArray
        End Get
    End Property
    Public Function AddBox(ByVal mode As TransformMode) As BoxItem
        Dim b As New BoxItem(Me, mode)
        SM.Box.Add(b)
        SM.UpdateBox(b)
        Me.RefreshInfoWithoutInvalidate()
        Me.Invalidate()
        Me.Update()
        Return b
    End Function
    Public Function AddBox(ByVal transformer As ITransformer) As BoxItem
        Dim b As New BoxItem(Me, transformer)
        SM.Box.Add(b)
        SM.UpdateBox(b)
        Me.RefreshInfoWithoutInvalidate()
        Me.Invalidate()
        Me.Update()
        Return b
    End Function
    Public Sub RemoveBox(ByVal index As Integer)
        SM.Box.RemoveAt(index)
        If SM.Box.Count = 0 Then
            Dim b As New BoxItem(Me, TransformMode.HexView)
            SM.Box.Add(b)
        End If
        Me.Invalidate()
        Me.Update()
    End Sub
    Public Sub RemoveBox(ByVal box As BoxItem)
        SM.Box.Remove(box)
        If SM.Box.Count = 0 Then
            Dim b As New BoxItem(Me, TransformMode.HexView)
            SM.Box.Add(b)
        End If
        Me.Invalidate()
        Me.Update()
    End Sub

    Public Property WriteMode As WriteMode
        Get
            Return SM.wMode
        End Get
        Set(value As WriteMode)
            If Not SM.wMode = value Then
                SM.wMode = value
                Me.Invalidate()
                Me.Update()
            End If
        End Set
    End Property
    <Browsable(False)>
    Public ReadOnly Property Styles As StyleSet
        Get
            Return SM
        End Get
    End Property
    Public Property CollumnHeaderUserPaint As Boolean
        Get
            Return SM.usrpaint_head
        End Get
        Set(value As Boolean)
            If Not SM.usrpaint_head = value Then
                SM.usrpaint_head = value
                Me.Invalidate()
                Me.Update()
            End If
        End Set
    End Property
    Public Property OffsetUserPaint As Boolean
        Get
            Return SM.usrpaint_offset
        End Get
        Set(value As Boolean)
            If Not SM.usrpaint_offset = value Then
                SM.usrpaint_offset = value
                Me.Invalidate()
                Me.Update()
            End If
        End Set
    End Property
    Public Property ContentUserPaint As Boolean
        Get
            Return SM.usrpaint_content
        End Get
        Set(value As Boolean)
            If Not SM.usrpaint_content = value Then
                SM.usrpaint_content = value
                Me.Invalidate()
                Me.Update()
            End If
        End Set
    End Property
    Public Property ExtraWidth As Integer
        Get
            Return extraW
        End Get
        Set(value As Integer)
            If value < 0 Then value = 0
            If Not extraW = value Then
                extraW = value
                RefreshInfo()
                Me.Update()
            End If
        End Set
    End Property
    Public Property OffsetHexSign As Boolean
        Get
            Return SM.hexSign
        End Get
        Set(value As Boolean)
            If Not SM.hexSign = value Then
                SM.hexSign = value
                SM.UpdateStyle()
                Me.RefreshInfoWithoutInvalidate()
                Me.Invalidate()
                Me.Update()
            End If
        End Set
    End Property
    Public Property OffsetLiteMode As Boolean
        Get
            Return SM.offsetLiteMode
        End Get
        Set(value As Boolean)
            If Not SM.offsetLiteMode = value Then
                SM.offsetLiteMode = value
                SM.UpdateStyle()
                Me.RefreshInfoWithoutInvalidate()
                Me.Invalidate()
                Me.Update()
            End If
        End Set
    End Property
    Public Property OffsetAutoSize As Boolean
        Get
            Return SM.offsetAutoSize
        End Get
        Set(value As Boolean)
            SM.offsetAutoSize = value
            If value Then
                SM.UpdateStyle()
                Me.RefreshInfoWithoutInvalidate()
                Me.Invalidate()
                Me.Update()
            End If
        End Set
    End Property
    Public Property OffsetWidth As Integer
        Get
            Return SM.offset_width
        End Get
        Set(value As Integer)
            If Not SM.offset_width = value And Not SM.offsetAutoSize Then
                SM.offset_width = value
                SM.UpdateStyle()
                Me.RefreshInfoWithoutInvalidate()
                Me.Invalidate()
                Me.Update()
            End If
        End Set
    End Property
    Public Property SelectionPower As Boolean
        Get
            Return selPower
        End Get
        Set(value As Boolean)
            selPower = value
        End Set
    End Property
    Public Property EnableContextMenu As Boolean
        Get
            Return enContextMenu
        End Get
        Set(value As Boolean)
            enContextMenu = value
        End Set
    End Property
    Public Sub AddTextStyle(ByVal ts As TextStyle)
        SM.AddTextStyle(ts)
    End Sub
    Public Sub OverrideTextStyle(ByVal ts As TextStyle)
        SM.OverrideTextStyle(ts)
    End Sub
    Public Sub SetBoxStyle(ByVal boxIndex As Integer, ByVal ts As BaseStyle)
        SM.Box(boxIndex).Style = ts
    End Sub
    Public Function GetBoxStyle(ByVal boxIndex As Integer) As BaseStyle
        Return SM.Box(boxIndex).Style
    End Function
    Public Function GetBoxStyle(ByVal box As BoxItem) As BaseStyle
        Return box.Style
    End Function
    Public Sub SetBoxStyle(ByVal box As BoxItem, ByVal ts As BaseStyle)
        box.Style = ts
    End Sub
    Public Sub ClearTextStyle(ByVal position As Long, ByVal length As Long, ByVal unit As PointUnit, ByVal target As StyleTarget, ByVal boxIndex As Integer())
        SM.ClearTextStyle(position, length, unit, target, boxIndex)
    End Sub
    Public Sub ClearTextStyles()
        SM.ClearTextStyles()
    End Sub
    Public Function GetTextStyle(ByVal position As Long, ByVal boxIndex As Integer, ByVal unit As PointUnit) As BaseStyle
        Return SM.GetTextStyle(position, boxIndex, unit)
    End Function
    Public Sub GetTextStyles()
        SM.GetTextStyles()
    End Sub
    Public Function GetText(ByVal boxIndex As Integer, ByVal position As Long, ByVal length As Long, ByVal unit As PointUnit) As String
        Dim box As BoxItem = SM.Box(boxIndex)
        Dim TI As AdvancedTransformInfo = Transformers.GetAdvancedTransformInfo(box.trans, SM.col_count, bb.GetLength + shift_val_pre)
        Dim trans As ITransformer = box.trans

        Dim LineA As Long = 0
        Dim LineB As Long = 0
        Dim ColA As Long = 0
        Dim ColB As Long = 0
        If unit = PointUnit.Byte Then
            LineA = Math.Floor(position / SM.col_count)
            LineB = Math.Floor((position + length) / SM.col_count)
            ColA = position Mod SM.col_count
            ColB = (position + length) Mod SM.col_count

            ColA = Math.Floor(ColA / trans.LengthPerData) * TI.PerData
            ColB = Math.Ceiling(ColB / trans.LengthPerData) * TI.PerData
        Else
            LineA = Math.Floor(position / TI.CharsPerRow)
            LineB = Math.Floor((position + length) / TI.CharsPerRow)
            ColA = position Mod TI.CharsPerRow
            ColB = (position + length) Mod TI.CharsPerRow

            ColA = Math.Floor(ColA / TI.PerData) * TI.PerData
            ColB = Math.Ceiling(ColB / TI.PerData) * TI.PerData

        End If
        If ColB = 0 And LineB > LineA Then
            LineB -= 1
        End If

        Dim QR As New QuickReader(bb)
        Dim rText As String = ""
        Dim subText As String = ""
        Dim AllText As String = ""
        Dim cFirst As Integer = ColA + 1
        Dim cLen As Integer = 0
        If LineA = LineB Then
            cLen = ColB - ColA
        Else
            cLen = TI.CharsPerRow
        End If
        For line As Long = LineA To LineB
            QR.Update(line * SM.col_count, SM.col_count)
            rText = GetRowText(QR, line, trans, False)

            subText = ("".PadLeft(cFirst - 1) & Mid(rText, cFirst, cLen)).PadRight(TI.CharsPerRow)

            If line = LineB Then
                AllText &= subText
            Else
                AllText &= subText & vbCrLf
            End If

            cLen = TI.CharsPerRow
            cFirst = 1
        Next
        Return AllText
    End Function
    Public Property EnableHotFind As Boolean
        Get
            Return dtoggleHotFind
        End Get
        Set(value As Boolean)
            If Not dtoggleHotFind = value Then
                dtoggleHotFind = value
                Me.Invalidate()
                Me.Update()
            End If
        End Set
    End Property
    Public Property ForceHotFind As Boolean
        Get
            Return ftoggleHotFind
        End Get
        Set(value As Boolean)
            If Not ftoggleHotFind = value Then
                ftoggleHotFind = value
                Me.Invalidate()
                Me.Update()
            End If
        End Set
    End Property
    Public Property ShowShiftSlider As Boolean
        Get
            Return slider_show
        End Get
        Set(value As Boolean)
            If Not slider_show = value Then
                slider_show = value
                Me.Invalidate()
                Me.Update()
            End If
        End Set
    End Property
    Public Property Shift As Long
        Get
            Dim oldVal As Long = slider_val
            If Not slider_push Then
                oldVal = -oldVal
            End If
            Return oldVal
        End Get
        Set(value As Long)
            Dim oldVal As Long = slider_val
            If Not slider_push Then
                oldVal = -oldVal
            End If

            If Not oldVal = value Then
                slider_push = (value > 0)
                shift_val = Math.Abs(value)
                Me.RefreshInfoWithoutInvalidate()
                Me.Invalidate()
                Me.Update()
            End If
        End Set
    End Property
#End Region

#Region "ByteBuilder Event Handler"
    Friend topLevelUD As UndoData
    Private Sub ByteBuilder_Load(sender As Object, e As EventArgs) Handles bb.Load
        Me.SL.Clear()
        RefreshInfo()
        Me.Invalidate()
        Me.Update()
    End Sub
    Private Sub ByteBuilder_NewUndoableState(sender As Object, e As UndoableByteBuilderEventArgs) Handles bb.NewUndoableState
        topLevelUD = New UndoData
        topLevelUD.SL = Me.SL.Clone
        topLevelUD.focussedBoxIndex = Me.FocussedBoxIndex
        topLevelUD.vscroll_val = Me.vscroll_val
        topLevelUD.hscroll_val = Me.hscroll_val
    End Sub
    Private Sub ByteBuilder_LoadUndoableState(sender As Object, e As UndoableByteBuilderEventArgs) Handles bb.LoadUndoableState
        Dim ud As UndoData = topLevelUD
        If Not IsNothing(e.Item.NextState) Then
            ud = e.Item.NextState.Data
            If IsNothing(ud) Then
                ud = topLevelUD
            End If
        End If

        Me.SL = ud.SL.Clone
        Me.FocussedBoxIndex = ud.focussedBoxIndex

        SetVScroll(ud.vscroll_val)
        SetHScroll(ud.hscroll_val)
        Me.Update()
    End Sub
    Private Sub ByteBuilder_ByteChanged(sender As Object, e As ByteChangedEventArgs) Handles bb.ByteChanged
        If e.Action = ActionTypes.Overwrite Then
            InvalidatePOS(e.Position, e.Length)
        ElseIf e.Action = ActionTypes.Insert Or e.Action = ActionTypes.Remove Then
            RefreshInfoWithoutInvalidate()
            InvalidatePOS(e.Position, bb.GetLength + shift_val_pre - e.Position)
            CheckSelection()
        End If

    End Sub

    Friend Sub CheckSelection()
        Dim max As Long = bb.GetLength + shift_val_pre

        Dim needInvalidate As Boolean = False
        For Each i In Me.SL.Items
            needInvalidate = False
            If i.anc > max Then
                i.anc = max
                needInvalidate = True
            End If
            If i.car > max Then
                i.car = max
                needInvalidate = True
            End If
        Next

        If needInvalidate Then
            Me.Invalidate()
        End If
    End Sub
#End Region

#Region "ScrollBars"
    Friend Sub VScrollBar_Show(ByVal visible As Boolean)
        USER32.ShowScrollBar(Me.Handle, USER32.ScrollBarDirection.SB_VERT, visible)
    End Sub
    Friend Sub VScrollBar_SetValue(ByVal value As Integer)
        Dim lpsi As New USER32.SCROLLINFO
        lpsi.cbSize = Marshal.SizeOf(lpsi)
        lpsi.fMask = USER32.ScrollInfoMask.SIF_ALL
        USER32.GetScrollInfo(Me.Handle, USER32.ScrollBarDirection.SB_VERT, lpsi)
        lpsi.nPos = value
        USER32.SetScrollInfo(Me.Handle, USER32.SBOrientation.SB_VERT, lpsi, True)
    End Sub
    Friend Sub VScrollBar_Seek(ByVal offset As Integer)
        Dim lpsi As New USER32.SCROLLINFO
        lpsi.cbSize = Marshal.SizeOf(lpsi)
        lpsi.fMask = USER32.ScrollInfoMask.SIF_ALL
        USER32.GetScrollInfo(Me.Handle, USER32.ScrollBarDirection.SB_VERT, lpsi)
        lpsi.nPos = lpsi.nPos + offset
        USER32.SetScrollInfo(Me.Handle, USER32.SBOrientation.SB_VERT, lpsi, True)
    End Sub
    Friend Sub VScrollBar_SetPage(ByVal value As Integer)
        Dim lpsi As New USER32.SCROLLINFO
        lpsi.cbSize = Marshal.SizeOf(lpsi)
        lpsi.fMask = USER32.ScrollInfoMask.SIF_ALL
        USER32.GetScrollInfo(Me.Handle, USER32.ScrollBarDirection.SB_VERT, lpsi)
        lpsi.nPage = value
        USER32.SetScrollInfo(Me.Handle, USER32.SBOrientation.SB_VERT, lpsi, True)
    End Sub
    Friend Sub VScrollBar_SetRange(ByVal nMin As Integer, ByVal nMax As Integer)
        Dim lpsi As New USER32.SCROLLINFO
        lpsi.cbSize = Marshal.SizeOf(lpsi)
        lpsi.fMask = USER32.ScrollInfoMask.SIF_ALL
        USER32.GetScrollInfo(Me.Handle, USER32.ScrollBarDirection.SB_VERT, lpsi)
        lpsi.nMin = nMin
        lpsi.nMax = nMax
        USER32.SetScrollInfo(Me.Handle, USER32.SBOrientation.SB_VERT, lpsi, True)
    End Sub
    Friend Sub VScrollBar_SetInfo(ByVal nMin As Integer, ByVal nMax As Integer, ByVal nPage As Integer)
        Dim lpsi As New USER32.SCROLLINFO
        lpsi.cbSize = Marshal.SizeOf(lpsi)
        lpsi.fMask = USER32.ScrollInfoMask.SIF_ALL
        USER32.GetScrollInfo(Me.Handle, USER32.ScrollBarDirection.SB_VERT, lpsi)
        lpsi.nMin = nMin
        lpsi.nMax = nMax
        lpsi.nPage = nPage
        USER32.SetScrollInfo(Me.Handle, USER32.SBOrientation.SB_VERT, lpsi, True)
    End Sub
    Friend Function VScrollBar_GetValue() As Integer
        Dim lpsi As New USER32.SCROLLINFO
        lpsi.cbSize = Marshal.SizeOf(lpsi)
        lpsi.fMask = USER32.ScrollInfoMask.SIF_ALL
        USER32.GetScrollInfo(Me.Handle, USER32.ScrollBarDirection.SB_VERT, lpsi)
        Return lpsi.nPos
    End Function
    Friend Function VScrollBar_GetRange() As Point
        Dim lpsi As New USER32.SCROLLINFO
        lpsi.cbSize = Marshal.SizeOf(lpsi)
        lpsi.fMask = USER32.ScrollInfoMask.SIF_ALL
        USER32.GetScrollInfo(Me.Handle, USER32.ScrollBarDirection.SB_VERT, lpsi)
        Return New Point(lpsi.nMin, lpsi.nMax)
    End Function
    Friend Function VScrollBar_GetPage() As Integer
        Dim lpsi As New USER32.SCROLLINFO
        lpsi.cbSize = Marshal.SizeOf(lpsi)
        lpsi.fMask = USER32.ScrollInfoMask.SIF_ALL
        USER32.GetScrollInfo(Me.Handle, USER32.ScrollBarDirection.SB_VERT, lpsi)
        Return lpsi.nPage
    End Function
    Friend Function VScrollBar_GetInfo() As USER32.SCROLLINFO
        Dim lpsi As New USER32.SCROLLINFO
        lpsi.cbSize = Marshal.SizeOf(lpsi)
        lpsi.fMask = USER32.ScrollInfoMask.SIF_ALL
        USER32.GetScrollInfo(Me.Handle, USER32.ScrollBarDirection.SB_VERT, lpsi)
        Return lpsi
    End Function
    Friend Sub HScrollBar_Show(ByVal visible As Boolean)
        USER32.ShowScrollBar(Me.Handle, USER32.ScrollBarDirection.SB_HORZ, visible)
    End Sub

    Friend Sub HScrollBar_SetValue(ByVal value As Integer)
        Dim lpsi As New USER32.SCROLLINFO
        lpsi.cbSize = Marshal.SizeOf(lpsi)
        lpsi.fMask = USER32.ScrollInfoMask.SIF_ALL
        USER32.GetScrollInfo(Me.Handle, USER32.ScrollBarDirection.SB_HORZ, lpsi)
        lpsi.nPos = value
        USER32.SetScrollInfo(Me.Handle, USER32.SBOrientation.SB_HORZ, lpsi, True)
    End Sub
    Friend Sub HScrollBar_Seek(ByVal offset As Integer)
        Dim lpsi As New USER32.SCROLLINFO
        lpsi.cbSize = Marshal.SizeOf(lpsi)
        lpsi.fMask = USER32.ScrollInfoMask.SIF_ALL
        USER32.GetScrollInfo(Me.Handle, USER32.ScrollBarDirection.SB_HORZ, lpsi)
        lpsi.nPos = lpsi.nPos + offset
        USER32.SetScrollInfo(Me.Handle, USER32.SBOrientation.SB_HORZ, lpsi, True)
    End Sub
    Friend Sub HScrollBar_SetPage(ByVal value As Integer)
        Dim lpsi As New USER32.SCROLLINFO
        lpsi.cbSize = Marshal.SizeOf(lpsi)
        lpsi.fMask = USER32.ScrollInfoMask.SIF_ALL
        USER32.GetScrollInfo(Me.Handle, USER32.ScrollBarDirection.SB_HORZ, lpsi)
        lpsi.nPage = value
        USER32.SetScrollInfo(Me.Handle, USER32.SBOrientation.SB_HORZ, lpsi, True)
    End Sub
    Friend Sub HScrollBar_SetRange(ByVal nMin As Integer, ByVal nMax As Integer)
        Dim lpsi As New USER32.SCROLLINFO
        lpsi.cbSize = Marshal.SizeOf(lpsi)
        lpsi.fMask = USER32.ScrollInfoMask.SIF_ALL
        USER32.GetScrollInfo(Me.Handle, USER32.ScrollBarDirection.SB_HORZ, lpsi)
        lpsi.nMin = nMin
        lpsi.nMax = nMax
        USER32.SetScrollInfo(Me.Handle, USER32.SBOrientation.SB_HORZ, lpsi, True)
    End Sub
    Friend Sub HScrollBar_SetInfo(ByVal nMin As Integer, ByVal nMax As Integer, ByVal nPage As Integer)
        Dim lpsi As New USER32.SCROLLINFO
        lpsi.cbSize = Marshal.SizeOf(lpsi)
        lpsi.fMask = USER32.ScrollInfoMask.SIF_ALL
        USER32.GetScrollInfo(Me.Handle, USER32.ScrollBarDirection.SB_HORZ, lpsi)
        lpsi.nMin = nMin
        lpsi.nMax = nMax
        lpsi.nPage = nPage
        USER32.SetScrollInfo(Me.Handle, USER32.SBOrientation.SB_HORZ, lpsi, True)
    End Sub
    Friend Function HScrollBar_GetValue() As Integer
        Dim lpsi As New USER32.SCROLLINFO
        lpsi.cbSize = Marshal.SizeOf(lpsi)
        lpsi.fMask = USER32.ScrollInfoMask.SIF_ALL
        USER32.GetScrollInfo(Me.Handle, USER32.ScrollBarDirection.SB_HORZ, lpsi)
        Return lpsi.nPos
    End Function
    Friend Function HScrollBar_GetRange() As Point
        Dim lpsi As New USER32.SCROLLINFO
        lpsi.cbSize = Marshal.SizeOf(lpsi)
        lpsi.fMask = USER32.ScrollInfoMask.SIF_ALL
        USER32.GetScrollInfo(Me.Handle, USER32.ScrollBarDirection.SB_HORZ, lpsi)
        Return New Point(lpsi.nMin, lpsi.nMax)
    End Function
    Friend Function HScrollBar_GetPage() As Integer
        Dim lpsi As New USER32.SCROLLINFO
        lpsi.cbSize = Marshal.SizeOf(lpsi)
        lpsi.fMask = USER32.ScrollInfoMask.SIF_ALL
        USER32.GetScrollInfo(Me.Handle, USER32.ScrollBarDirection.SB_HORZ, lpsi)
        Return lpsi.nPage
    End Function
    Friend Function HScrollBar_GetInfo() As USER32.SCROLLINFO
        Dim lpsi As New USER32.SCROLLINFO
        lpsi.cbSize = Marshal.SizeOf(lpsi)
        lpsi.fMask = USER32.ScrollInfoMask.SIF_ALL
        USER32.GetScrollInfo(Me.Handle, USER32.ScrollBarDirection.SB_HORZ, lpsi)
        Return lpsi
    End Function
    Friend Sub RefreshVScrollBar()
        Dim afterLength As Long = bb.GetLength + shift_val_pre
        Dim maxLength As Long = Math.Ceiling(afterLength / SM.col_count)
        Dim vShow As Boolean = False
        If maxLength > limit_vscroll Then
            vscroll_enhanced = True
            vscroll_max_value = maxLength
            vscroll_max_scroll = limit_vscroll
            VScrollBar_SetInfo(0, vscroll_max_scroll, ShowedRowCount)
            VScrollBar_Show(True)
            vShow = True
        ElseIf maxLength < ShowedRowCount Then
            vscroll_enhanced = False
            vscroll_max_value = maxLength
            vscroll_max_scroll = maxLength
            VScrollBar_SetInfo(0, vscroll_max_scroll, ShowedRowCount)
            VScrollBar_Show(False)
            ShowedStart = 0
        Else
            vscroll_enhanced = False
            vscroll_max_value = maxLength
            vscroll_max_scroll = maxLength
            VScrollBar_SetInfo(0, vscroll_max_scroll, ShowedRowCount)
            VScrollBar_Show(True)
            vShow = True
        End If
    End Sub
    Friend Sub RefreshHScrollBar()
        Dim inv As Boolean = False
        'hscroll
        Dim wcontent As Integer = SM.all_padLeft + 25 + extraW
        If Not SM.offset_mode = OffsetMode.Hidden Then
            wcontent += SM.offset_width
        End If

        For Each i In SM.Box
            wcontent += i.w
        Next
        Dim vShow As Boolean = False
        If MyRect.Width < wcontent Then
            HScrollBar_SetInfo(0, wcontent, MyRect.Width)
            If HScrollBar_GetValue() > wcontent Then
                HScrollBar_SetValue(wcontent)
            End If
            HScrollBar_Show(True)
            inv = True
            vShow = True
        Else
            HScrollBar_SetValue(0)
            HScrollBar_Show(False)
        End If
    End Sub
    Private Sub HexBox_Scroll(sender As Object, e As ScrollEventArgs) Handles MyBase.Scroll
        If e.ScrollOrientation = ScrollOrientation.VerticalScroll Then
            If e.Type = ScrollEventType.SmallIncrement Then
                VScrollBar_SetValue(VScrollBar_GetValue() + 1)
            ElseIf e.Type = ScrollEventType.SmallDecrement Then
                VScrollBar_SetValue(VScrollBar_GetValue() - 1)
            ElseIf e.Type = ScrollEventType.First Then
                VScrollBar_SetValue(0)
            ElseIf e.Type = ScrollEventType.Last Then
                VScrollBar_SetValue(0)
            ElseIf e.Type = ScrollEventType.ThumbTrack Then
                VScrollBar_SetValue(e.NewValue)
            ElseIf e.Type = ScrollEventType.LargeIncrement Then
                VScrollBar_SetValue(VScrollBar_GetValue() + VScrollBar_GetPage())
            ElseIf e.Type = ScrollEventType.LargeDecrement Then
                VScrollBar_SetValue(VScrollBar_GetValue() - VScrollBar_GetPage())
            ElseIf e.Type = ScrollEventType.ThumbPosition Then
                VScrollBar_SetValue(VScrollBar_GetValue())
            End If

            Dim v As Long = VScrollBar_GetValue()
            If vscroll_enhanced Then
                v = v / vscroll_max_scroll * vscroll_max_value
            End If
            If Not vscroll_val = v Then
                vscroll_val = v
                ShowedStart = vscroll_val * SM.col_count
                Dim rect As New Rectangle(0, 0, GetContentWidth, MyRect.Height)
                Me.Invalidate(rect)
                Me.Update()
            End If

        ElseIf e.ScrollOrientation = ScrollOrientation.HorizontalScroll Then
            If e.Type = ScrollEventType.SmallIncrement Then
                HScrollBar_SetValue(HScrollBar_GetValue() + 1)
            ElseIf e.Type = ScrollEventType.SmallDecrement Then
                HScrollBar_SetValue(HScrollBar_GetValue() - 1)
            ElseIf e.Type = ScrollEventType.First Then
                HScrollBar_SetValue(0)
            ElseIf e.Type = ScrollEventType.Last Then
                HScrollBar_SetValue(0)
            ElseIf e.Type = ScrollEventType.ThumbTrack Then
                HScrollBar_SetValue(e.NewValue)
            ElseIf e.Type = ScrollEventType.LargeIncrement Then
                HScrollBar_SetValue(HScrollBar_GetValue() + HScrollBar_GetPage())
            ElseIf e.Type = ScrollEventType.LargeDecrement Then
                HScrollBar_SetValue(HScrollBar_GetValue() - HScrollBar_GetPage())
            ElseIf e.Type = ScrollEventType.ThumbPosition Then
                HScrollBar_SetValue(HScrollBar_GetValue())
            End If

            Dim v As Integer = HScrollBar_GetValue()
            If Not hscroll_val = v Then
                hscroll_val = v
                Me.Invalidate()
                Me.Update()
            End If

        End If
    End Sub

    Private Sub HexBox_MouseWheel(sender As Object, e As MouseEventArgs) Handles MyBase.MouseWheel
        Dim v As Long = vscroll_val + (e.Delta / -120)
        If v < 0 Then v = 0
        SetVScroll(v)
    End Sub
    Friend Function SetVScroll(ByVal value As Long) As Long
        Dim v As Long = value
        If vscroll_enhanced Then
            value = value / vscroll_max_value * vscroll_max_scroll
        End If
        VScrollBar_SetValue(value)

        Dim vNow As Long = VScrollBar_GetValue()
        Dim pg As Long = vscroll_max_scroll - ShowedRowCount + 1
        If vNow >= pg Then
            If vscroll_enhanced Then
                v = vNow / vscroll_max_scroll * vscroll_max_value
            Else
                v = vNow
            End If
        End If

        Dim inv As Boolean = False
        If Not vscroll_val = v Then
            vscroll_val = v
            ShowedStart = vscroll_val * SM.col_count
            inv = True
        End If

        If inv Then
            Dim rect As New Rectangle(0, 0, GetContentWidth, MyRect.Height)
            Me.Invalidate(rect)
        End If
        Return v
    End Function
    Friend Function SetHScroll(ByVal value As Integer) As Integer
        HScrollBar_SetValue(value)
        Dim v As Integer = HScrollBar_GetValue()
        If Not hscroll_val = v Then
            hscroll_val = v
            Me.Invalidate()
            Me.Update()
        End If
        Return v
    End Function
    Friend Function GetContentWidth() As Integer
        Dim wcontent As Integer = 0
        If Not SM.offset_mode = OffsetMode.Hidden Then
            wcontent += SM.offset_width
        End If
        For Each i In SM.Box
            wcontent += i.w
        Next
        Return wcontent
    End Function

#End Region

#Region "Edit"
    Public Property [ReadOnly] As Boolean
        Get
            Return Not ro
        End Get
        Set(value As Boolean)
            ro = value
        End Set
    End Property
    Public ReadOnly Property IsLastCopyOverflow As Boolean
        Get
            Return last_copy_overflow
        End Get
    End Property
    Friend Property MaxCopyStringLengthAllowed As Integer
        Get
            Return maxcp
        End Get
        Set(value As Integer)
            If value < 1 Then value = 1
            maxcp = value
        End Set
    End Property
    Friend ReadOnly Property CanEdit As Boolean
        Get
            Return Not ro
        End Get
    End Property
    Friend ReadOnly Property CanResize As Boolean
        Get
            Return True
        End Get
    End Property
    Friend ReadOnly Property CanSelectAll As Boolean
        Get
            Return Not bb.GetLength + shift_val_pre = 0
        End Get
    End Property
    Friend ReadOnly Property CanClear As Boolean
        Get
            Return Not ro
        End Get
    End Property
    Friend ReadOnly Property CanExpand As Boolean
        Get
            Return CanEdit And CanResize
        End Get
    End Property
    <Browsable(False)>
    Public ReadOnly Property CanUndo As Boolean
        Get
            Return CanEdit And CanResize And bb.Undoable.CanUndo
        End Get
    End Property
    <Browsable(False)>
    Public ReadOnly Property CanRedo As Boolean
        Get
            Return CanEdit And CanResize And bb.Undoable.CanRedo
        End Get
    End Property
    <Browsable(False)>
    Public ReadOnly Property CanCut As Boolean
        Get
            Return CanEdit And CanResize And Not SL.SelectionLength = 0
        End Get
    End Property
    <Browsable(False)>
    Public ReadOnly Property CanCopy As Boolean
        Get
            Return Not SL.SelectionLength = 0
        End Get
    End Property
    <Browsable(False)>
    Public ReadOnly Property CanPaste As Boolean
        Get
            If Not CanEdit And CanResize Then Return False

            If Not CheckPasteData() Then Return False

            Return True
        End Get
    End Property
    Friend Function CheckPasteData() As Boolean
        Dim ido As IDataObject = Clipboard.GetDataObject
        If IsNothing(ido) Then Return False

        If ido.GetDataPresent(DataFormats.StringFormat) Then
            Dim data As String = ido.GetData(DataFormats.StringFormat)
            If SM.wMode = WriteMode.Insert Then
                Return CheckDataInsert(data, Nothing)
            Else
                Return CheckData(data)
            End If
        ElseIf ido.GetDataPresent(DataFormats.Text) Then
            Dim data As String = ido.GetData(DataFormats.StringFormat)
            If SM.wMode = WriteMode.Insert Then
                Return CheckDataInsert(data, Nothing)
            Else
                Return CheckData(data)
            End If
        ElseIf ido.GetDataPresent(DataFormats.UnicodeText) Then
            Dim data As String = ido.GetData(DataFormats.StringFormat)
            If SM.wMode = WriteMode.Insert Then
                Return CheckDataInsert(data, Nothing)
            Else
                Return CheckData(data)
            End If
        ElseIf ido.GetDataPresent(DataFormats.FileDrop) Then

        Else
            Return False
        End If

        Return False
    End Function
    Friend Function CheckData(ByVal data As String) As Boolean
        If IsNothing(data) Then Return False
        If data.Length = 0 Then Return False
        Dim afterLength As Long = bb.GetLength + shift_val_pre
        Dim box As BoxItem = SelectedBox
        Dim trans As ITransformer = box.trans
        Dim TI As AdvancedTransformInfo = Transformers.GetAdvancedTransformInfo(trans, SM.col_count, afterLength)

        Dim sItem As SelectionItem = SL.Curent
        Dim anc As Long = sItem.anc
        Dim dlen As Integer = data.Length
        Dim line As Long = Math.Floor(anc / TI.CharsPerRow)
        Dim col As Integer = anc Mod TI.CharsPerRow

        Dim rowText As Char()
        Dim rText As New List(Of Char)
        Dim pastedText As String = ""
        Dim newText As String = ""
        Dim newBytes(SM.col_count - 1) As Byte
        Dim str As Integer = col
        Dim ix As Integer = 0
        Dim rRest As Integer = 0
        Dim curentLen As Long = 0
        Do
            rowText = GetRowText(QR, line, box, False)
            rText.Clear()
            rText.AddRange(rowText)
            pastedText = Mid(data, ix + 1, TI.CharsPerRow - str)

            curentLen = afterLength - line * SM.col_count

            If SM.wMode = WriteMode.Insert Then
                rText.InsertRange(str, pastedText)
            Else
                For i As Integer = 0 To pastedText.Length - 1
                    rText(str + i) = pastedText(i)
                Next
            End If

            newText = rText.ToArray
            newText = Mid(newText, 1, TI.CharsPerRow)

            rRest = GetRowBytes(newText, TI, newBytes)

            If Not rRest = SM.col_count Then
                If curentLen = rRest Then
                    If str >= rRest * TI.PerData Then
                        Return False
                    End If
                ElseIf curentLen < rRest Then
                Else
                    Return False
                End If
            End If

            str = 0
            ix += pastedText.Length
            line += 1
        Loop Until ix >= dlen

        Return True
    End Function
    Friend Function CheckDataInsert(ByVal data As String, ByRef resultb As Byte()) As Boolean
        If IsNothing(data) Then Return False
        If data.Length = 0 Then Return False
        Dim afterLength As Long = bb.GetLength + shift_val_pre
        Dim box As BoxItem = SelectedBox
        Dim trans As ITransformer = box.trans
        Dim TI As AdvancedTransformInfo = Transformers.GetAdvancedTransformInfo(trans, SM.col_count, afterLength)

        Dim dlen As Integer = data.Length
        Dim line As Integer = Math.Floor(dlen / TI.CharsPerRow)
        Dim col As Integer = dlen Mod TI.CharsPerRow
        Dim col_t As Integer = Math.Floor(col / TI.PerData)
        Dim col_d As Integer = 0
        If Not col_t < TI.DataCount And Not TI.PartialCount = 0 Then
            col_d = TI.DataCount * trans.LengthPerData + Math.Floor((col - col_t * TI.PerData) / 2) * 2
        Else
            col_d = Math.Ceiling(col / TI.PerData) * trans.LengthPerData
            If col_d > SM.col_count Then col_d = SM.col_count
        End If

        Dim abs_len As Integer = line * SM.col_count + col_d
        Dim b(abs_len - 1) As Byte
        Dim b2 As Byte() = Nothing
        Dim a As Integer = 0

        Dim index As Integer = 0
        Dim subText As String = ""
        Dim r As Integer = 0
        Dim isPartial As Boolean = False
        Dim di As Integer = 0

        For y As Integer = 0 To line
            For x As Integer = 0 To TI.CharsPerRow - 1 Step TI.PerData
                index = y * TI.CharsPerRow + x
                If index >= dlen Then Exit For
                r = TI.PerData
                If x + r > TI.CharsPerRow Then
                    r = TI.CharsPerRow - x
                    isPartial = True
                Else
                    isPartial = False
                End If
                If index + r > dlen Then
                    isPartial = True
                End If

                subText = Mid(data, index + 1, r)

                If isPartial Then
                    ReDim b2(subText.Length / 2 - 1)
                    Try
                        For i As Integer = 0 To subText.Length - 1 Step 2
                            b2(i) = Mid(subText, i * 2 + 1, 2)
                        Next
                    Catch ex As Exception
                        Return False
                    End Try
                Else
                    Try
                        b2 = trans.GetBytes(subText)
                    Catch ex As Exception
                        Return False
                    End Try
                End If
                If Not IsNothing(b2) Then
                    For i As Integer = 0 To b2.Length - 1
                        b(di + i) = b2(i)
                    Next
                    di += b2.Length
                End If
            Next
        Next
        resultb = b
        Return True
    End Function
    Friend Function GetRowBytes(ByVal text As String, TI As AdvancedTransformInfo, ByRef b As Byte()) As Integer
        Dim isPartial As Boolean = False
        Dim r As Integer = 0
        Dim dlen As Integer = text.Length
        Dim subText As String = ""
        Dim b2 As Byte() = Nothing
        Dim ix As Integer = 0
        For i As Integer = 0 To TI.CharsPerRow - 1 Step TI.PerData
            r = TI.PerData
            If i + r >= TI.CharsPerRow Then
                isPartial = True
                r = TI.CharsPerRow - i - 1
            Else
                isPartial = False
            End If
            If i + r >= dlen Then
                isPartial = True
                r = dlen - i
            End If

            subText = Mid(text, i + 1, r)
            If isPartial Then
                ReDim b2(subText.Length / 2 - 1)
                Try
                    For i2 As Integer = 0 To subText.Length - 1 Step 2
                        b2(i2) = "&H" & Mid(subText, i2 * 2 + 1, 2)
                    Next
                Catch ex As Exception
                    Return ix
                End Try
            Else
                Try
                    b2 = TI.Trans.GetBytes(subText)
                Catch ex As Exception
                    Return ix
                End Try
            End If
            If Not IsNothing(b2) Then
                For i2 As Integer = 0 To b2.Length - 1
                    b(ix + i2) = b2(i2)
                Next
                ix += b2.Length
            End If
        Next
        Return ix
    End Function
    <Browsable(False)>
    Public ReadOnly Property CanDelete As Boolean
        Get
            Return CanEdit And CanResize And Not SL.SelectionLength = 0
        End Get
    End Property
    Public Sub Expand()
        If Not CanExpand Then Exit Sub
    End Sub
    Public Sub Undo()
        If Not CanUndo Then Exit Sub

        bb.Undoable.Undo()

        Me.Invalidate()
    End Sub
    Public Sub Redo()
        If Not CanRedo Then Exit Sub

        bb.Undoable.Redo()
        Me.Invalidate()
    End Sub
    Public Sub Cut()
        If Not CanCut Then Exit Sub
        Copy()
        Delete(1)
    End Sub
    Public Sub Copy()
        If Not CanCopy Then Exit Sub

        Dim SL As SelectionManager = Me.SL.ArrangeSelection

        Dim afterLength As Long = bb.GetLength + shift_val_pre
        Dim targetBox As BoxItem = SelectedBox
        Dim targetTrans As ITransformer = targetBox.trans
        Dim TI As AdvancedTransformInfo = Transformers.GetAdvancedTransformInfo(targetTrans, SM.col_count, afterLength)

        Dim TIs(SM.Box.Count - 1) As AdvancedTransformInfo
        Dim TIss As AdvancedTransformInfo = Nothing

        If CopyMode.HasFlag(CopyModes.AllContents) Then
            For i As Integer = 0 To SM.Box.Count - 1
                TIs(i) = Transformers.GetAdvancedTransformInfo(SM.Box(i).trans, SM.col_count, afterLength)
            Next
        End If

        Dim QR As New QuickReader(bb)
        Dim LastBox As BoxItem = SM.Box.Last
        Dim AllText As String = ""
        Dim multilines As Boolean = CopyMode.HasFlag(CopyModes.Multilines)

        Dim cpr As Integer = SM.offset_mode
        If SM.hexSign Then
            cpr += 1
        End If

        Dim adrLen As Integer = 0
        Dim firstX As Boolean = False

        Dim countLines As Long = 1
        For Each i In SL.Items
            Dim LineA As Long = Math.Floor(i.SelectionStart / TI.CharsPerRow)
            Dim LineB As Long = Math.Floor((i.SelectionStart + i.SelectionLength) / TI.CharsPerRow)
            countLines += LineB - LineA
        Next

        For Each i In SL.Items
            Dim LineA As Long = Math.Floor(i.SelectionStart / TI.CharsPerRow)
            Dim ColA As Long = i.SelectionStart Mod TI.CharsPerRow
            Dim LineB As Long = Math.Floor((i.SelectionStart + i.SelectionLength) / TI.CharsPerRow)
            Dim ColB As Long = (i.SelectionStart + i.SelectionLength) Mod TI.CharsPerRow
            Dim cFirst As Integer = ColA + 1
            Dim cLen As Integer = 0
            Dim cFirst2 As Integer = 0
            Dim cLen2 As Integer = 0

            Dim rText As String = ""
            Dim subText As String = ""
            Dim adrText As String = ""
            If CopyModes.ANSICopy <= CopyMode And CopyMode <= CopyModes.ArrayOfBytesCopy Then
                Dim bytStart As Long = LineA * SM.col_count + Math.Floor(ColA / TI.PerData) * targetTrans.LengthPerData
                Dim bytEnd As Long = LineB * SM.col_count + Math.Ceiling(ColB / TI.PerData) * targetTrans.LengthPerData

                If bytEnd >= afterLength Then
                    bytEnd = afterLength - 1
                End If

                Dim cpm As Integer = 0
                Dim enc As System.Text.Encoding = Nothing
                If CopyMode = CopyModes.UnicodeCopy Then
                    enc = Helper.uni
                    cpm = 1
                ElseIf CopyMode = CopyModes.ANSICopy Then
                    enc = Helper.ansi
                    cpm = 2
                ElseIf CopyMode = CopyModes.ArrayOfBytesCopy Then
                    cpm = 3
                End If

                Dim ix As Long = bytStart
                Do
                    Dim r As Integer = QR.Update(ix, QR.MaxLength)
                    If r = 0 Then Exit Do
                    If (ix + r) >= bytEnd Then
                        r = bytEnd - ix
                    End If
                    If cpm = 3 Then
                        For index As Integer = QR.Position To QR.Position + r - 1
                            subText &= Helper.Hex2(QR.Buffer(index)) & " "
                        Next
                    Else
                        subText = enc.GetString(QR.Buffer, ix - QR.Position, r)
                    End If
                    AllText &= subText
                    If AllText.Length >= maxcp Then
                        last_copy_overflow = True
                        Exit Do
                    End If

                    ix += r
                Loop Until ix >= bytEnd
                If AllText.Length >= maxcp Then
                    last_copy_overflow = True
                    Exit For
                End If
            Else
                adrLen = 0
                If SM.offset_mode <= 16 Then
                    adrLen = Hex(LineB).Length + 6
                Else
                    adrLen = LineB.ToString.Length + 6
                End If
                If SM.hexSign Then
                    adrLen += 1
                End If

                For y As Long = LineA To LineB
                    subText = ""
                    adrText = ""

                    If CopyMode.HasFlag(CopyModes.WithOffset) Then
                        If SM.offset_mode <= 16 Then
                            If SM.offsetLiteMode Then
                                adrText = Hex(y * SM.col_count)
                                If SM.hexSign Then
                                    adrText &= "h"
                                End If
                                adrText = adrText.PadRight(adrLen)
                            Else
                                adrText = Helper.HexN(y * SM.col_count, SM.offset_mode)
                                If SM.hexSign Then
                                    adrText &= "h"
                                End If
                                adrText &= "     "
                            End If

                        Else
                            Dim adr As Long = y * SM.col_count
                            Dim adrb As Byte() = BitConverter.GetBytes(adr)
                            Select Case SM.offset_mode
                                Case OffsetMode.Bytes1
                                    adrText = adrb.First
                                Case OffsetMode.Bytes2
                                    adrText = BitConverter.ToUInt16(adrb, 0)
                                Case OffsetMode.Bytes4
                                    adrText = BitConverter.ToUInt32(adrb, 0)
                                Case OffsetMode.Bytes8
                                    adrText = adr
                            End Select
                            If Not SM.offsetLiteMode Then
                                adrText = adrText.PadLeft(cpr)
                                adrText &= "     "
                            Else
                                adrText = adrText.PadRight(adrLen)
                            End If
                        End If

                    End If

                    If Not firstX Then
                        firstX = True
                        If CopyMode.HasFlag(CopyModes.WithCollumnHeader) Then
                            Dim headerText As String = "".PadRight(adrText.Length)

                            For i2 As Integer = 0 To SM.Box.Count - 1
                                TIss = TIs(i2)

                                Dim nHex As Integer = TIss.PerData
                                If nHex > 2 Then
                                    nHex = 2
                                End If

                                Dim Tx As String = ""
                                For ix As Integer = 0 To SM.col_count - 1 Step TIss.Trans.LengthPerData
                                    Tx &= Helper.HexN(ix, nHex).PadRight(TIss.PerData)
                                Next

                                headerText &= Mid(Tx, 1, TIss.CharsPerRow).PadRight(TIss.CharsPerRow)
                                If Not SM.Box(i2) Is LastBox Then
                                    headerText &= "     "
                                End If
                            Next

                            AllText = headerText & vbCrLf & vbCrLf
                        End If

                    End If

                    If y = LineB Then
                        cLen = ColB - cFirst + 1
                    Else
                        cLen = TI.CharsPerRow
                    End If

                    If Not cLen = 0 Then
                        subText = adrText
                        QR.Update(y * SM.col_count, TI.CharsPerRow)

                        If CopyMode.HasFlag(CopyModes.AllContents) Then
                            For Each box In SM.Box
                                TIss = TIs(box.Index)
                                rText = GetRowText(QR, y, box.trans, True)
                                If box Is targetBox Then
                                    subText &= ("".PadLeft(cFirst - 1) & Mid(rText, cFirst, cLen)).PadRight(TIss.CharsPerRow)
                                Else
                                    If Not cFirst = 1 Then
                                        cFirst2 = Math.Floor((Math.Floor((cFirst - 1) / TI.PerData) * targetTrans.LengthPerData) / box.trans.LengthPerData) * TIss.PerData + 1
                                    Else
                                        cFirst2 = 1
                                    End If

                                    cLen2 = Math.Ceiling((Math.Ceiling((cLen - 1) / TI.PerData) * targetTrans.LengthPerData) / box.trans.LengthPerData) * TIss.PerData + 1
                                    If cLen2 > TIss.CharsPerRow Then
                                        cLen2 = TIss.CharsPerRow
                                    End If

                                    subText &= ("".PadLeft(cFirst2 - 1) & Mid(rText, cFirst2, cLen2)).PadRight(TIss.CharsPerRow)
                                End If

                                If Not cLen = 0 And Not LastBox Is box Then subText &= "     "
                            Next
                            AllText &= subText
                            If y < LineB Then AllText &= vbCrLf
                        Else
                            If multilines And Not countLines = 1 Then
                                rText = GetRowText(QR, y, targetTrans, True)
                                subText &= ("".PadLeft(cFirst - 1) & Mid(rText, cFirst, cLen)).PadRight(TI.CharsPerRow)
                            Else
                                rText = GetRowText(QR, y, targetTrans, False)
                                subText &= Mid(rText, cFirst, cLen)
                            End If
                            AllText &= subText
                            If y < LineB And multilines And Not countLines = 1 Then AllText &= vbCrLf
                        End If
                    End If

                    If AllText.Length >= maxcp Then
                        last_copy_overflow = True
                        Exit For
                    End If
                    cFirst = 1
                Next
            End If
            If AllText.Length >= maxcp Then
                last_copy_overflow = True
                Exit For
            End If
        Next

        If AllText.Length > maxcp Then
            last_copy_overflow = True
            Clipboard.SetText(Mid(AllText, 1, maxcp)) ' & vbCrLf & vbCrLf & "<Limited copy for : " & maxcp & " characters>" & vbCrLf & "Please check " & Chr(34) & "MaxCopyStringLengthAllowed" & Chr(34) & " property for detail.")
        Else
            last_copy_overflow = False
            Clipboard.SetText(AllText)
        End If
    End Sub
    Public Sub Paste()
        If Not CanPaste Then Exit Sub

        Dim ido As IDataObject = Clipboard.GetDataObject
        If ido.GetDataPresent(DataFormats.StringFormat) Then
            Dim data As String = ido.GetData(DataFormats.StringFormat)
            Dim anc As Long = SL.Curent.anc
            If SM.wMode = WriteMode.Insert Then
                PasteTextInsert(data)
            Else
                PasteText(data)
            End If
            SL.Curent.anc = anc
            SL.Curent.car = anc + data.Length

            Dim TI As AdvancedTransformInfo = Transformers.GetAdvancedTransformInfo(SelectedBox.trans, SM.col_count, bb.GetLength - shift_val_pre)
            If SL.Curent.car > TI.MaxAllCharsLength Then
                SL.Curent.car = TI.MaxAllCharsLength
            End If
        ElseIf ido.GetDataPresent(DataFormats.Text) Then
            Dim data As String = ido.GetData(DataFormats.StringFormat)
            Dim anc As Long = SL.Curent.anc
            If SM.wMode = WriteMode.Insert Then
                PasteTextInsert(data)
            Else
                PasteText(data)
            End If
            SL.Curent.anc = anc
            SL.Curent.car = anc + data.Length

            Dim TI As AdvancedTransformInfo = Transformers.GetAdvancedTransformInfo(SelectedBox.trans, SM.col_count, bb.GetLength - shift_val_pre)
            If SL.Curent.car > TI.MaxAllCharsLength Then
                SL.Curent.car = TI.MaxAllCharsLength
            End If
        ElseIf ido.GetDataPresent(DataFormats.UnicodeText) Then
            Dim data As String = ido.GetData(DataFormats.StringFormat)
            Dim anc As Long = SL.Curent.anc
            If SM.wMode = WriteMode.Insert Then
                PasteTextInsert(data)
            Else
                PasteText(data)
            End If
            SL.Curent.anc = anc
            SL.Curent.car = anc + data.Length

            Dim TI As AdvancedTransformInfo = Transformers.GetAdvancedTransformInfo(SelectedBox.trans, SM.col_count, bb.GetLength - shift_val_pre)
            If SL.Curent.car > TI.MaxAllCharsLength Then
                SL.Curent.car = TI.MaxAllCharsLength
            End If
        ElseIf ido.GetDataPresent(DataFormats.FileDrop) Then

        Else

        End If

    End Sub
    Friend Sub PasteTextInsert(ByVal data As String)
        Dim b As Byte() = Nothing
        If CheckDataInsert(data, b) Then
            Dim afterLength As Long = bb.GetLength + shift_val_pre
            Dim box As BoxItem = SelectedBox
            Dim trans As ITransformer = box.trans
            Dim TI As AdvancedTransformInfo = Transformers.GetAdvancedTransformInfo(trans, SM.col_count, afterLength)

            Dim abs_pos As Long = ToPosition(SL.Curent.anc, TI)
            bb.Insert(abs_pos, b)

        End If
    End Sub
    Friend Sub PasteText(ByVal data As String)
        If IsNothing(data) Then Exit Sub
        If data.Length = 0 Then Exit Sub
        Dim afterLength As Long = bb.GetLength + shift_val_pre
        Dim box As BoxItem = SelectedBox
        Dim trans As ITransformer = box.trans
        Dim TI As AdvancedTransformInfo = Transformers.GetAdvancedTransformInfo(trans, SM.col_count, afterLength)

        Dim sItem As SelectionItem = SL.Curent
        Dim anc As Long = sItem.anc
        Dim dlen As Integer = data.Length
        Dim line As Long = Math.Floor(anc / TI.CharsPerRow)
        Dim col As Integer = anc Mod TI.CharsPerRow

        Dim rowText As Char()
        Dim rText As New List(Of Char)
        Dim pastedText As String = ""
        Dim newText As String = ""
        Dim str As Integer = col
        Dim ix As Integer = 0
        Dim rRest As Integer = 0
        Dim curentLen As Long = 0
        Do
            rowText = GetRowText(QR, line, box, False)
            rText.Clear()
            rText.AddRange(rowText)
            pastedText = Mid(data, ix + 1, TI.CharsPerRow - str)

            curentLen = afterLength - line * SM.col_count

            For i As Integer = 0 To pastedText.Length - 1
                rText(str + i) = pastedText(i)
            Next

            newText = rText.ToArray
            newText = Mid(newText, 1, TI.CharsPerRow)

            Dim newBytes(SM.col_count - 1) As Byte
            rRest = GetRowBytes(newText, TI, newBytes)


            If Not rRest = SM.col_count Then
                ReDim Preserve newBytes(rRest - 1)
                bb.Overwrite(line * SM.col_count, newBytes)
                If curentLen = rRest Then
                    If str >= rRest * TI.PerData Then
                        Exit Sub
                    End If
                ElseIf curentLen < rRest Then
                Else
                    Exit Sub
                End If
            Else
                bb.Overwrite(line * SM.col_count, newBytes)
            End If

            str = 0
            ix += pastedText.Length
            line += 1
        Loop Until ix >= dlen
    End Sub
    Public Sub Delete()
        Delete(0)
    End Sub
    Public Sub Clear()
        If Not CanClear Then Exit Sub

        Dim afterLength As Long = bb.GetLength + shift_val_pre
        Dim targetBox As BoxItem = SelectedBox
        Dim targetTrans As ITransformer = targetBox.trans

        Dim snapped As SelectionManager = Me.SL.ArrangeSelection
        Dim snapped2 As SelectionManager = snapped.Clone
        Dim TI As AdvancedTransformInfo = Transformers.GetAdvancedTransformInfo(targetTrans, SM.col_count, afterLength)
        Dim mf As New ModifyFormats
        mf.RecordUndoable = False
        mf.EnableUpdateEventHandler = False

        Dim c As Integer = snapped.itms.Count - 1
        For i As Integer = 0 To c
            Dim item As SelectionItem = snapped.itms(i)
            Dim LineA As Long = Math.Floor(item.SelectionStart / TI.CharsPerRow)
            Dim ColA As Long = item.SelectionStart Mod TI.CharsPerRow
            Dim LineB As Long = Math.Floor((item.SelectionStart + item.SelectionLength) / TI.CharsPerRow)
            Dim ColB As Long = (item.SelectionStart + item.SelectionLength) Mod TI.CharsPerRow
            Dim bytStart As Long = LineA * SM.col_count + Math.Floor(ColA / TI.PerData) * targetTrans.LengthPerData
            Dim bytEnd As Long = LineB * SM.col_count + Math.Ceiling(ColB / TI.PerData) * targetTrans.LengthPerData

            If i = c Then
                mf = New ModifyFormats
                mf.RecordUndoable = True
                mf.EnableUpdateEventHandler = True

                Dim ud As New UndoData
                ud.SL = snapped2
                ud.vscroll_val = Me.vscroll_val
                ud.hscroll_val = Me.hscroll_val
                ud.focussedBoxIndex = Me.FocussedBoxIndex
                mf.UndoableData = ud
                mf.UndoableName = "Clear"
            End If

            Dim ln As Long = bytEnd - bytStart
            Dim st As Integer = UShort.MaxValue
            If st > ln Then st = ln
            Dim dt(st - 1) As Byte
            For ix As Long = 0 To ln - 1 Step st
                If ln - ix < st Then
                    Dim dt2((ln - ix) - 1) As Byte
                    dt = dt2
                End If
                bb.Overwrite(bytStart, dt, mf)
            Next
        Next

        Me.Invalidate()
        Me.Update()
    End Sub
    Friend Sub Delete(parentMethod As Integer)
        If Not CanDelete Then Exit Sub

        Dim afterLength As Long = bb.GetLength + shift_val_pre
        Dim targetBox As BoxItem = SelectedBox
        Dim targetTrans As ITransformer = targetBox.trans

        Dim snapped As SelectionManager = Me.SL.ArrangeSelection
        Dim snapped2 As SelectionManager = snapped.Clone
        Dim TI As AdvancedTransformInfo = Transformers.GetAdvancedTransformInfo(targetTrans, SM.col_count, afterLength)
        Dim mf As New ModifyFormats
        mf.RecordUndoable = False
        mf.EnableUpdateEventHandler = False

        Dim caret As Long = Me.SL.Curent.Anchor
        Me.SL.Clear()
        Me.SL.Curent.anc = caret
        Me.SL.Curent.car = caret

        Dim c As Integer = snapped.itms.Count - 1
        For i As Integer = 0 To c
            Dim item As SelectionItem = snapped.itms(i)
            Dim LineA As Long = Math.Floor(item.SelectionStart / TI.CharsPerRow)
            Dim ColA As Long = item.SelectionStart Mod TI.CharsPerRow
            Dim LineB As Long = Math.Floor((item.SelectionStart + item.SelectionLength) / TI.CharsPerRow)
            Dim ColB As Long = (item.SelectionStart + item.SelectionLength) Mod TI.CharsPerRow
            Dim bytStart As Long = LineA * SM.col_count + Math.Floor(ColA / TI.PerData) * targetTrans.LengthPerData
            Dim bytEnd As Long = LineB * SM.col_count + Math.Ceiling(ColB / TI.PerData) * targetTrans.LengthPerData

            If i = c Then
                mf = New ModifyFormats
                mf.RecordUndoable = True
                mf.EnableUpdateEventHandler = True

                Dim ud As New UndoData
                ud.SL = snapped2
                ud.vscroll_val = Me.vscroll_val
                ud.hscroll_val = Me.hscroll_val
                ud.focussedBoxIndex = Me.FocussedBoxIndex
                mf.UndoableData = ud

                If parentMethod = 0 Then
                    mf.UndoableName = "Delete"
                ElseIf parentMethod = 1 Then
                    mf.UndoableName = "Cut"
                ElseIf parentMethod = 2 Then
                    mf.UndoableName = "Paste"
                    mf.RecordUndoable = False
                End If
            End If

            bb.Remove(bytStart, bytEnd - bytStart, mf)

            Dim snd As Long = item.SelectionStart + item.SelectionLength
            Dim len As Long = 0
            For i2 As Integer = i + 1 To c
                Dim item2 As SelectionItem = snapped.itms(i2)
                If item2.SelectionStart >= snd Then
                    len = item2.SelectionLength
                    item2.anc -= snd
                    item2.car = item2.anc + len
                End If
            Next
        Next

        Me.Invalidate()
        Me.Update()
    End Sub
    Public Sub SelectAll()
        If Not CanSelectAll Then Exit Sub

        Me.SL.Clear()
        Dim TI As AdvancedTransformInfo = Transformers.GetAdvancedTransformInfo(Me.SL.trans, SM.col_count, bb.GetLength + shift_val_pre)
        Dim LF As Boolean = False
        Dim col As Integer = TI.MaxAllCharsLength Mod TI.CharsPerRow
        If col = 0 And Not TI.MaxAllCharsLength = 0 Then
            LF = True
        End If

        Me.SL.AddSelection(0, TI.MaxAllCharsLength, LF)
        EnsureVisibleSelection()
        Me.Invalidate()
    End Sub
    Friend Sub AddMenu(ByVal list As List(Of MenuItem), ByVal menu As String())
        For i As Integer = 0 To menu.Length - 1
            If menu(i) = "|" Then
                Dim m As New MenuItem(USER32.MenuMasks.MIIM_ID, "")
                m.Separator = True
                list.Add(m)
            Else
                Dim m As New MenuItem(USER32.MenuMasks.MIIM_STRING, menu(i))
                list.Add(m)
            End If
        Next
    End Sub

#End Region
End Class

Friend Class UndoData
    Public SL As SelectionManager
    Public vscroll_val As Long
    Public hscroll_val As Long
    Public focussedBoxIndex As Integer

End Class

Public Class HexBoxSelectionEventArgs
    Inherits EventArgs
    Public ReadOnly Property OldBoxIndex As Integer
    Public ReadOnly Property NewBoxIndex As Integer
    Public ReadOnly Property OldSelection As SelectionManager
    Public ReadOnly Property NewSelection As SelectionManager
    Public Property Cancel As Boolean
    Public Sub New(OldBoxIndex As Integer, OldSelection As SelectionManager, NewBoxIndex As Integer, NewSelection As SelectionManager)
        Me.OldSelection = OldSelection
        Me.NewSelection = NewSelection
        Me.OldBoxIndex = OldBoxIndex
        Me.NewBoxIndex = NewBoxIndex
    End Sub
End Class

Public Class GDI32PaintEventArgs
    Inherits EventArgs

    Public ReadOnly Property hDC As IntPtr
    Public ReadOnly Property ClipRectangle As Rectangle
    Public ReadOnly Property Translate As Point

    Public Sub New(hDC As IntPtr, ClipRectangle As Rectangle, Translate As Point)
        Me.hDC = hDC
        Me.ClipRectangle = ClipRectangle
        Me.Translate = Translate
    End Sub
End Class

Public Class ContentPaintEventArgs
    Inherits EventArgs

    Public ReadOnly Property Row As Long
    Public ReadOnly Property Text As String
    Public ReadOnly Property Bytes As Byte()
    Public ReadOnly Property hDC As IntPtr
    Public ReadOnly Property Bounds As Rectangle
    Public ReadOnly Property Translate As Point
    Public ReadOnly Property Styles As List(Of RenderGroup)
    Public Property DefaultPaint As Boolean

    Public Sub New(hDC As IntPtr, Bounds As Rectangle, Translate As Point, Row As Long, Text As String, Bytes As Byte(), Styles As List(Of RenderGroup))
        Me.hDC = hDC
        Me.Bounds = Bounds
        Me.Translate = Translate
        Me.Row = Row
        Me.Text = Text
        Me.Bytes = Bytes
        Me.Styles = Styles
    End Sub
End Class

Friend Class RenderParam
    Public hDC As IntPtr
    Public translate As Point
    Public size As Size
    Public AfterLength As Long
    Public A As Long
    Public WContent As Integer
    Public Function IsVisible(ByVal rect As Rectangle) As Boolean
        Return GDI32.RectVisible(hDC, rect)
    End Function
    Public Function IsVisible(ByVal rect As RectangleF) As Boolean
        Return GDI32.RectVisible(hDC, Rectangle.Round(rect))
    End Function
    Public Function IsAbsoluteVisible(ByVal rect As Rectangle) As Boolean
        Return GDI32.PtVisible(hDC, rect.Left, rect.Top) And GDI32.PtVisible(hDC, rect.Left, rect.Top) And GDI32.PtVisible(hDC, rect.Right, rect.Top) And GDI32.PtVisible(hDC, rect.Right, rect.Bottom)
    End Function
End Class
Friend Class RenderBox
    Public Box As BoxItem
    Public BoxIndex As Integer
    Public SMBackRGB As Integer
    Public SMTextRGB As Integer
    Public SMSelTextRGB As Integer
    Public SMSelBackRGB As Integer
    Public SMOvrTextRGB As Integer
    Public SMOvrBackRGB As Integer
    Public SMOvrText2RGB As Integer
    Public SMOvrBack2RGB As Integer
    Public SMOvrText3RGB As Integer
    Public SMOvrBack3RGB As Integer
    Public SMGrayTextRGB As Integer
    Public SMGrayBackRGB As Integer
    Public SMWrongTextRGB As Integer
    Public SMUBTextRGB As Integer
    Public SMUETextRGB As Integer
    Public SMBackBlendRGB As Integer
    Public rBack As Color
    Public rSel As Color
    Public rBackS As Color
    Public rSelS As Color
    Public rFontS As Font
    Public SL As SelectionManager
    Public CharsPerRow As Integer
    Public colFloor As Integer
    Public perData As Integer
    Public partialChars As Integer
    Public afterLength As Long
    Public TI As AdvancedTransformInfo
End Class
Public Class RenderChar
    Public TextColor As Integer
    Public BackColor As Integer
    Public Font As Font
    Public IsPartial As Boolean
    Public ReadOnly Property hFont As IntPtr
        Get
            Return HexBox.GetFontData(Font).hFont
        End Get
    End Property
    Public Sub Reset()
        TextColor = 0
        BackColor = 0
        Font = Nothing
        IsPartial = False
    End Sub
End Class
Public Class RenderGroup
    Public Start As Integer
    Public Length As Integer
    Public Style As RenderChar
    Public Width As Single
    Public Height As Single

End Class
Friend Class DragAttachment
    Implements IDataObject

    Friend hb As HexBox
    Friend Shared list As New List(Of String)
    Friend Shared myFormat As String = getMyFormat()
    Friend Shared maxString As Integer = 100

    Private Shared Function getMyFormat() As String
        Dim myFormat As String = GetType(DragAttachment).ToString
        list.Add(DataFormats.StringFormat)
        list.Add(DataFormats.Text)
        list.Add(DataFormats.UnicodeText)
        list.Add(DataFormats.FileDrop)
        list.Add(myFormat)
        Return myFormat
    End Function
    Public Sub New(ByVal hb As HexBox)
        Me.hb = hb
    End Sub

    Public Function GetData(format As String, autoConvert As Boolean) As Object Implements IDataObject.GetData
        Try
            Select Case format
                Case DataFormats.StringFormat, DataFormats.Text, DataFormats.UnicodeText
                    Dim tx As String = ""
                    Dim trans As ITransformer = hb.SM.Box(hb.FocussedBoxIndex).trans
                    Dim charsPerRow As Integer = hb.GetCharsPerRow(hb.SM.Box(hb.FocussedBoxIndex))
                    Dim perData As Integer = trans.CharsPerData + trans.Sparator
                    Dim QR As New QuickReader(hb.bb)
                    Dim cpr As Integer = hb.SM.offset_mode
                    If hb.SM.hexSign Then
                        cpr += 1
                    End If

                    For Each i In hb.SL.Items
                        Dim lineStart As Long = Math.Floor(i.SelectionStart / charsPerRow)
                        Dim lineEnd As Long = Math.Floor((i.SelectionStart + i.SelectionLength) / charsPerRow)
                        Dim adrText As String = ""
                        Dim rowText As String = ""
                        Dim r As Integer = 0

                        For ln As Long = lineStart To lineEnd - 1
                            If hb.SM.offset_mode <= 16 Then
                                If hb.SM.offsetLiteMode Then
                                    adrText = Hex(ln * hb.SM.col_count)
                                    If hb.SM.hexSign Then
                                        adrText &= "h"
                                    End If
                                Else
                                    adrText = Helper.HexN(ln * hb.SM.col_count, hb.SM.offset_mode)
                                    If hb.SM.hexSign Then
                                        adrText &= "h"
                                    End If
                                End If
                            Else
                                Dim adr As Long = ln * hb.SM.col_count
                                Dim adrb As Byte() = BitConverter.GetBytes(adr)
                                Select Case hb.SM.offset_mode
                                    Case OffsetMode.Bytes1
                                        adrText = adrb.First
                                    Case OffsetMode.Bytes2
                                        adrText = BitConverter.ToUInt16(adrb, 0)
                                    Case OffsetMode.Bytes4
                                        adrText = BitConverter.ToUInt32(adrb, 0)
                                    Case OffsetMode.Bytes8
                                        adrText = adr
                                End Select
                                If Not hb.SM.offsetLiteMode Then
                                    adrText = adrText.PadLeft(cpr)
                                End If
                            End If
                            r = QR.Update(ln * hb.SM.col_count, hb.SM.col_count)
                            rowText = ""
                            Dim BL As Integer = hb.SM.Box.Count - 1
                            For ibox As Integer = 0 To BL
                                If Not ibox = BL Then
                                    rowText &= hb.GetRowText(QR, ln, hb.SM.Box(ibox).trans, True) & "     "
                                Else
                                    rowText &= hb.GetRowText(QR, ln, hb.SM.Box(ibox).trans, True)
                                End If
                            Next
                            tx &= adrText & "     " & rowText & vbCrLf
                        Next
                    Next
                    Return tx
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

    Public Function GetData(format As String) As Object Implements IDataObject.GetData
        Return GetData(format, True)
    End Function

    Public Function GetData(format As Type) As Object Implements IDataObject.GetData
        Return GetData(format.ToString, True)
    End Function

    Public Sub SetData(format As String, autoConvert As Boolean, data As Object) Implements IDataObject.SetData
        Throw New NotImplementedException()
    End Sub

    Public Sub SetData(format As String, data As Object) Implements IDataObject.SetData
        Throw New NotImplementedException()
    End Sub

    Public Sub SetData(format As Type, data As Object) Implements IDataObject.SetData
        Throw New NotImplementedException()
    End Sub

    Public Sub SetData(data As Object) Implements IDataObject.SetData
        Throw New NotImplementedException()
    End Sub

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
        Return list.ToArray
    End Function

    Public Function GetFormats() As String() Implements IDataObject.GetFormats
        Return GetFormats(True)
    End Function
End Class
Friend Class MenuItem
    Public Property Text As String
    Public Property Flag As USER32.MenuMasks
    Public hb As HexBox
    Public Property Enabled As Boolean = True
    Public Property Separator As Boolean
    Public Property ID As Integer
    Public Property Items As New List(Of MenuItem)
    Public Sub New(ByVal flag As USER32.MenuMasks, ByVal text As String)
        Me.Flag = flag
        Me.Text = text
    End Sub
End Class
Public Enum CopyModes As Integer
    SelectedContent = 0
    Multilines = 1
    AllContents = (2 Or Multilines)
    WithOffset = (4 Or Multilines)
    WithCollumnHeader = (8 Or Multilines)
    AllContentsWithOffset = SelectedContent Or Multilines Or AllContents Or WithOffset
    Full = SelectedContent Or Multilines Or AllContents Or WithOffset Or WithCollumnHeader
    ANSICopy = 32
    UnicodeCopy = 33
    ArrayOfBytesCopy = 34
End Enum
