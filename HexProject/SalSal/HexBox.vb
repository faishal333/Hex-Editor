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
    Friend SL As New SelectionManager(Me)

    Friend ShowedStart As Long
    Friend ShowedRowCount As Integer
    Friend ShowedLength As Integer

    Friend MyRect As Rectangle
    Friend DrawInvalidateArea As Boolean ' = True

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
    Public Event SelectionChanged(ByVal sender As Object, ByVal e As HexBoxSelectionEventArgs)
    Public Event GDI32Paint(ByVal sender As Object, ByVal e As GDI32PaintEventArgs)
    Public Event ContentPaint(ByVal sender As Object, ByVal e As ContentPaintEventArgs)

    <Browsable(False)>
    Public ReadOnly Property ByteBuilder As SalSal.ByteBuilder
        Get
            Return bb
        End Get
    End Property

    Dim ansi As System.Text.Encoding = System.Text.Encoding.Default
    Public Sub New()

        ' This call is required by the designer.
        InitializeComponent()

        ' Add any initialization after the InitializeComponent() call.

        Dim fs As New MemoryStream
        bb = New SalSal.ByteBuilder(fs)
        QR = New QuickReader(bb)

        SetStyle(ControlStyles.UserPaint, True)
        SetStyle(ControlStyles.ContainerControl, False)
        SetStyle(ControlStyles.Selectable, True)
        'SetStyle(USER32.WindowStyles.WS_VSCROLL, True)
        'SetStyle(USER32.WindowStyles.WS_HSCROLL, True)
        TimerIndicator.Start()

    End Sub

#Region "WndProc"
    Friend Const WM_SETFOCUS As Integer = &H7
    Protected Overrides Sub WndProc(ByRef m As Message)
        Select Case m.Msg
            Case WM_SETFOCUS
                Exit Sub
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
        If Not SM.header_mode = HeaderMode.Hidden Then hh = SM.header_height + SM.content_ypad
        Dim showed_row_count As Long = Math.Floor((MyRect.Height - hh) / SM.row_height) + 1
        If showed_row_count < 1 Then showed_row_count = 1
        Dim showed_count As Long = showed_row_count * SM.col_count
        Me.ShowedStart = show_str
        Me.ShowedRowCount = showed_row_count
        Me.ShowedLength = showed_count
        'vscroll
        RefreshVScrollBar()
        If vscroll_enhanced Then
            vscroll_val = VScrollBar_GetValue() / maxScroll * vscroll_max
        Else
            vscroll_val = VScrollBar_GetValue()
        End If

        'hscroll
        RefreshHScrollBar()
        hscroll_val = HScrollBar_GetValue()
    End Sub

#Region "Drawing"
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

    Protected Overrides Sub OnPaintBackground(e As PaintEventArgs)

    End Sub
    Protected Overrides Sub OnPaint(e As PaintEventArgs)
        'Dim dt As Date = Date.Now
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
        RM.AfterLength = bb.GetLength
        RM.WContent = GetContentWidth()
        RM.A = vscroll_val * SM.col_count
        Dim rl As Integer = QR.Update(RM.A, ShowedLength)

        RenderAll(RM)
        Dim evt As New GDI32PaintEventArgs(hDC, e.ClipRectangle, RM.translate)
        RaiseEvent GDI32Paint(Me, evt)

        Dim hDCDest As IntPtr = e.Graphics.GetHdc
        GDI32.BitBlt(hDCDest, e.ClipRectangle.X, e.ClipRectangle.Y, bmp.Width, bmp.Height, hDC, 0, 0, GDI32.TernaryRasterOperations.SRCCOPY)

        'gbmp.ReleaseHdc(hDC)
        e.Graphics.ReleaseHdc(hDCDest)
        gbmp.Dispose()
        bmp.Dispose()

        If DrawInvalidateArea Then
            Dim br As New SolidBrush(Color.FromArgb(150, Rnd() * 255, Rnd() * 255, Rnd() * 255))
            e.Graphics.FillRectangle(br, 0, 0, MyRect.Width, MyRect.Height)
            br.Dispose()
        End If
        'Dim ln As Long = (Date.Now - dt).TotalMilliseconds
        'e.Graphics.FillRectangle(Brushes.White, 0, 0, 100, 20)
        'e.Graphics.DrawString(ln, SM.Font, Brushes.Red, 0, 0)

    End Sub

    Friend Sub RenderAll(e As RenderParam)
        RenderBackground(e)
        If Not SM.usrpaint_head Then RenderCollumnHeader(e)
        If Not SM.usrpaint_offset Then RenderOffset(e)
        RenderContents(e)
        If SM.draw_border Then RenderBorder(e)
    End Sub

    Friend Sub RenderBackground(e As RenderParam)
        Dim hBrush As IntPtr = GDI32.CreateSolidBrush(Helper.GetRGB(SM.bkClr))
        Dim hPen As IntPtr = GDI32.GetStockObject(GDI32.StockObjects.NULL_PEN)

        Dim hBrushPrevious As IntPtr = GDI32.SelectObject(e.hDC, hBrush)
        Dim hPenPrevious As IntPtr = GDI32.SelectObject(e.hDC, hPen)

        GDI32.Rectangle(e.hDC, 0, 0, e.size.Width + 1, e.size.Height + 1)

        GDI32.SelectObject(e.hDC, hBrushPrevious)
        GDI32.SelectObject(e.hDC, hPenPrevious)

        GDI32.DeleteObject(hBrush)
    End Sub
    Friend Sub RenderCollumnHeader(e As RenderParam)
        If SM.header_mode = HeaderMode.Hidden Then Exit Sub
        Dim rect As New Rectangle(SM.all_xpad, SM.all_ypad, e.WContent, SM.header_height)

        If rect.Height <= 0 Then Exit Sub
        rect.X += e.translate.X
        rect.Y += e.translate.Y

        Dim hDC As IntPtr = e.hDC

        If Not e.IsVisible(rect) Then Exit Sub

        Dim fd As FontData = Nothing

        Dim hState As Integer = GDI32.SaveDC(hDC)
        Dim hRgn As IntPtr = GDI32.CreateRectRgn(rect.X, rect.Y - 1, rect.Right + SM.offsetBox_width, rect.Bottom + SM.header_ypad)
        GDI32.SelectClipRgn(hDC, hRgn)

        Dim hBrush As IntPtr = GDI32.CreateSolidBrush(Helper.GetRGB(SM.headbkClr))
        Dim hPen As IntPtr = GDI32.GetStockObject(GDI32.StockObjects.NULL_PEN)

        Dim hBrushPrevious As IntPtr = GDI32.SelectObject(e.hDC, hBrush)
        Dim hPenPrevious As IntPtr = GDI32.SelectObject(e.hDC, hPen)

        GDI32.Rectangle(hDC, rect.X, rect.Y, rect.Right + 1, rect.Y + SM.header_height + 1)

        GDI32.SelectObject(e.hDC, hBrushPrevious)
        GDI32.SelectObject(e.hDC, hPenPrevious)

        GDI32.DeleteObject(hBrush)
        If Not SM.offset_mode = OffsetMode.Hidden Then
            rect.X += SM.offsetBox_width
        End If
        rect.Y += SM.header_ypad
        rect.Height = SM.header_height

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
            rect.X += box.xpad

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
                rectX.X = rect.X + rX
                rectX.Y = rect.Y
                subTx = Mid(Tx, i.Start, i.Length)
                fd = GetFontData(i.Style.Font)
                rectX.Width = Helper.GetTextWidth(fd.ABC, subTx)
                rectX.Height = SM.row_height

                If e.IsVisible(New Rectangle(rectX.X - 1, rectX.Y, rectX.Width, rectX.Height)) Then
                    GDI32.SelectObject(hDC, fd.hFont)
                    GDI32.SetTextColor(hDC, i.Style.TextColor)
                    GDI32.SetBackColor(hDC, i.Style.BackColor)
                    GDI32.TextOut(hDC, rect.X + rX, rect.Y, subTx, subTx.Length)
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

        Dim rect As New Rectangle(SM.all_xpad, SM.all_ypad, SM.offsetBox_width, MyRect.Height)

        If rect.Height <= 0 Then Exit Sub
        rect.X += e.translate.X
        rect.Y += e.translate.Y

        Dim hDC As IntPtr = e.hDC

        If Not e.IsVisible(rect) Then Exit Sub

        Dim fd As FontData = Nothing

        If Not SM.header_mode = HeaderMode.Hidden Then
            rect.Y += SM.header_height
        End If
        Dim hState As Integer = GDI32.SaveDC(hDC)
        Dim hRgn As IntPtr = GDI32.CreateRectRgn(rect.X, rect.Y - 1, rect.Right, rect.Bottom + SM.content_ypad + SM.header_height)
        GDI32.SelectClipRgn(hDC, hRgn)

        Dim hBrush As IntPtr = GDI32.CreateSolidBrush(Helper.GetRGB(SM.offsetbkClr))
        Dim hPen As IntPtr = GDI32.GetStockObject(GDI32.StockObjects.NULL_PEN)

        Dim hBrushPrevious As IntPtr = GDI32.SelectObject(e.hDC, hBrush)
        Dim hPenPrevious As IntPtr = GDI32.SelectObject(e.hDC, hPen)

        GDI32.Rectangle(hDC, rect.X, rect.Y, rect.X + SM.offsetBox_width + 1, rect.Y + MyRect.Height)

        GDI32.SelectObject(e.hDC, hBrushPrevious)
        GDI32.SelectObject(e.hDC, hPenPrevious)

        GDI32.DeleteObject(hBrush)

        Dim SMTextClr As Integer = Helper.GetRGB(SM.offsettxClr)
        Dim SMBackClr As Integer = Helper.GetRGB(SM.offsetbkClr)
        Dim blendClr As Integer = Helper.GetRGB(Helper.Blend(SM.highLightLineColor, SM.offsetbkClr, SM.highLightLineColor.A / 255))

        hBrush = GDI32.CreateSolidBrush(blendClr)
        hPen = GDI32.GetStockObject(GDI32.StockObjects.NULL_PEN)

        hBrushPrevious = GDI32.SelectObject(e.hDC, hBrush)
        hPenPrevious = GDI32.SelectObject(e.hDC, hPen)

        If Not SM.header_mode = HeaderMode.Hidden Then
            rect.Y += SM.content_ypad
        End If

        rect.Height = SM.row_height

        Dim drawAddressStart As Long = Me.ShowedStart
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

        If SM.offsetAutoSize Then
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
            Dim rW As Integer = Helper.GetTextWidth(fd.ABC, offsetS) + SM.offset_xpad * 2 + 1
            If Not SM.offsetBox_width = rW Then
                SM.offsetBox_width = rW
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

        For i As Integer = 0 To ShowedRowCount - 1
            If e.IsVisible(rect) Then
                N = (vscroll_val + i) * SM.col_count
                If N < e.AfterLength Or e.AfterLength = 0 And i = 0 Then
                    If SM.offset_mode <= 16 Then
                        If SM.offsetAutoSize Then
                            Tx = Hex(drawAddressStart + i * SM.col_count)
                            If SM.hexSign Then
                                    Tx &= "h"
                                End If
                                Tx = Tx.PadLeft(cpr)
                            Else
                                Tx = Helper.HexN(drawAddressStart + i * SM.col_count, SM.offset_mode)
                            If SM.hexSign Then
                                Tx &= "h"
                            End If
                        End If
                    Else
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

                    If isHighLight Then GDI32.Rectangle(hDC, rect.X, rect.Y, rect.X + SM.offsetBox_width + 1, rect.Y + SM.row_height + 1)

                    Dim w As Single = 0
                    Dim rX As Single = 0
                    For Each i2 In nGroups
                        subTx = Mid(Tx, i2.Start, i2.Length)
                        fd = GetFontData(i2.Style.Font)
                        w = Helper.GetTextWidth(fd.ABC, subTx)

                        GDI32.SelectObject(hDC, fd.hFont)
                        GDI32.SetTextColor(hDC, i2.Style.TextColor)
                        GDI32.SetBackColor(hDC, i2.Style.BackColor)
                        GDI32.TextOut(hDC, rect.X + SM.offset_xpad + rX, rect.Y, subTx, i2.Length)
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


        GDI32.SelectObject(e.hDC, hBrushPrevious)
        GDI32.SelectObject(e.hDC, hPenPrevious)

        GDI32.DeleteObject(hBrush)

        GDI32.RestoreDC(hDC, hState)
        GDI32.DeleteObject(hRgn)

    End Sub
    Friend Sub RenderContents(e As RenderParam)
        Dim rect As New Rectangle(SM.all_xpad, SM.all_ypad, e.WContent, MyRect.Height)
        If Not SM.offset_mode = OffsetMode.Hidden Then
            rect.X += SM.offsetBox_width
        End If

        If rect.Height <= 0 Then Exit Sub
        rect.X += e.translate.X
        rect.Y += e.translate.Y

        Dim hDC As IntPtr = e.hDC

        If Not e.IsVisible(rect) Then Exit Sub

        Dim fd As FontData = Nothing
        If Not SM.header_mode = HeaderMode.Hidden Then
            rect.Y += SM.header_height + SM.content_ypad
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

        Dim SMBackRGB As Integer ' = Helper.GetRGB(SM.bkClr)
        Dim SMTextRGB As Integer ' = Helper.GetRGB(Helper.Blend(SM.txClr, SM.bkClr, SM.txClr.A / 255))
        Dim SMSelTextRGB As Integer ' = Helper.GetRGB(Helper.Blend(SM.seltxClr, SM.bkClr, SM.seltxClr.A / 255))
        Dim SMSelBackRGB As Integer ' = Helper.GetRGB(Helper.Blend(SM.selbkClr, SM.bkClr, SM.selbkClr.A / 255))
        Dim SMOvrTextRGB As Integer ' = Helper.GetRGB(Helper.Blend(SM.ovrtxClr, SM.bkClr, SM.ovrtxClr.A / 255))
        Dim SMOvrBackRGB As Integer '= Helper.GetRGB(Helper.Blend(SM.ovrbkClr, SM.bkClr, SM.ovrbkClr.A / 255))
        Dim SMOvrText2RGB As Integer ' = Helper.GetRGB(Helper.Blend(SM.ovrtx2Clr, SM.bkClr, SM.ovrtx2Clr.A / 255))
        Dim SMOvrBack2RGB As Integer ' = Helper.GetRGB(Helper.Blend(SM.ovrbk2Clr, SM.bkClr, SM.ovrbk2Clr.A / 255))
        Dim SMGrayTextRGB As Integer ' = Helper.GetRGB(Helper.Blend(SM.GrayTextColor, SM.bkClr, SM.GrayTextColor.A / 255))
        Dim SMGrayBackRGB As Integer = Helper.GetRGB(Color.Red)
        Dim SMBackBlendRGB As Integer '= Helper.GetRGB(Helper.Blend(SM.highLightLineColor, SM.bkClr, SM.highLightLineColor.A / 255))
        Dim rBackS As Color = Nothing
        Dim rSelS As Color = Nothing
        Dim rFontS As Font = Nothing
        For boxIndex As Integer = 0 To SM.Box.Count - 1
            Dim box As BoxItem = SM.Box(boxIndex)
            Dim SL As SelectionManager = Nothing
            If sourceBox Is box Then
                SL = Me.SL
            Else
                SL = Me.SL.TransformSelection(sourceBox.trans, box.trans)
            End If

            If IsNothing(box.Style) Then
                SMBackRGB = Helper.GetRGB(SM.bkClr)
                SMTextRGB = Helper.GetRGB(Helper.Blend(SM.txClr, SM.bkClr, SM.txClr.A / 255))
                SMSelTextRGB = Helper.GetRGB(Helper.Blend(SM.seltxClr, SM.bkClr, SM.seltxClr.A / 255))
                SMSelBackRGB = Helper.GetRGB(Helper.Blend(SM.selbkClr, SM.bkClr, SM.selbkClr.A / 255))
                SMOvrTextRGB = Helper.GetRGB(Helper.Blend(SM.ovrtxClr, SM.bkClr, SM.ovrtxClr.A / 255))
                SMOvrBackRGB = Helper.GetRGB(Helper.Blend(SM.ovrbkClr, SM.bkClr, SM.ovrbkClr.A / 255))
                SMOvrText2RGB = Helper.GetRGB(Helper.Blend(SM.ovrtx2Clr, SM.bkClr, SM.ovrtx2Clr.A / 255))
                SMOvrBack2RGB = Helper.GetRGB(Helper.Blend(SM.ovrbk2Clr, SM.bkClr, SM.ovrbk2Clr.A / 255))
                SMGrayTextRGB = Helper.GetRGB(Helper.Blend(SM.graytxClr, SM.bkClr, SM.graytxClr.A / 255))
                SMBackBlendRGB = Helper.GetRGB(Helper.Blend(SM.highLightLineColor, SM.bkClr, SM.highLightLineColor.A / 255))
                rBackS = SM.bkClr
                rSelS = SM.selbkClr
                rFontS = SM.Font
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
                rFontS = sts.Font

                SMBackRGB = Helper.GetRGB(sts.BackColor)
                SMTextRGB = Helper.GetRGB(Helper.Blend(sts.TextColor, sts.BackColor, sts.TextColor.A / 255))
                SMSelTextRGB = Helper.GetRGB(Helper.Blend(sts.HightlightTextColor, sts.BackColor, sts.HightlightTextColor.A / 255))
                SMSelBackRGB = Helper.GetRGB(Helper.Blend(sts.HightlightBackColor, sts.BackColor, sts.HightlightBackColor.A / 255))
                SMOvrTextRGB = Helper.GetRGB(Helper.Blend(sts.HotTextColor, sts.BackColor, sts.HotTextColor.A / 255))
                SMOvrBackRGB = Helper.GetRGB(Helper.Blend(sts.HotBackColor, sts.BackColor, sts.HotBackColor.A / 255))
                SMOvrText2RGB = Helper.GetRGB(Helper.Blend(sts.HotTextColor2, sts.BackColor, sts.HotTextColor2.A / 255))
                SMOvrBack2RGB = Helper.GetRGB(Helper.Blend(sts.HotBackColor2, sts.BackColor, sts.HotBackColor2.A / 255))
                SMGrayTextRGB = Helper.GetRGB(Helper.Blend(sts.GrayTextColor, sts.BackColor, sts.GrayTextColor.A / 255))
                SMBackBlendRGB = Helper.GetRGB(Helper.Blend(SM.highLightLineColor, sts.BackColor, SM.highLightLineColor.A / 255))
                rBackS = sts.BackColor
                rSelS = sts.HightlightBackColor
            End If


            Dim N As Long = 0
            Dim Ix As Integer = 0
            Dim Tx As String = ""
            Dim subTx As String = ""
            Dim Idx As Long = 0
            Dim Iy As Integer = 0

            rect.Y = bY
            rect.X += box.xpad

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

            Dim isSel As Boolean = False
            Dim isContains As Boolean = False
            Dim isIndicator As Boolean = False
            Dim rBack As Color = Nothing
            Dim rSel As Color = Nothing

            For y As Integer = 0 To ShowedRowCount - 1
                Ix = (e.A - QR.Position) + y * SM.col_count

                N = (vscroll_val + y) * SM.col_count
                isHighLight = N <= index And index < (N + SM.col_count)
                isHighLight = isHighLight And SM.highlightLine

                Tx = ""
                Iy = 0
                For i As Integer = 0 To SM.col_count - 1 Step box.trans.LengthPerData
                    If i >= colFloor Then
                        For i2 As Integer = colFloor To SM.col_count - 1
                            If (Ix + i) < QR.Length Then
                                Tx &= Helper.Hex2(QR.Buffer(Ix + i2))
                            Else
                                Tx &= "".PadRight(2)
                            End If
                        Next
                        Tx &= "".PadRight(box.trans.Sparator)
                    Else
                        If (Ix + i) < QR.Length And (Ix + i) + box.trans.LengthPerData <= QR.Length Then
                            Tx &= box.trans.GetString(QR.Buffer, Ix + i).PadRight(perData)
                        ElseIf (Ix + i) < QR.Length Then
                            For i2 As Integer = 0 To (Ix + i) + box.trans.LengthPerData - QR.Length - 1
                                If (Ix + i) < QR.Length Then
                                    Tx &= Helper.Hex2(QR.Buffer(Ix + i + i2)).PadRight(box.trans.CharsPerData)
                                Else
                                    Tx &= "".PadRight(2)
                                End If
                            Next
                            Tx &= "".PadRight(box.trans.Sparator)
                        Else
                            Tx &= "".PadRight(perData)
                        End If
                    End If
                    Iy += perData
                Next

                Iy = 0
                Dim iZ As Integer = 0
                For i As Integer = 0 To charsPerRow - 1
                    Idx = (e.A + y * SM.col_count) / SM.col_count * charsPerRow + i

                    rMap = rMaps(i)
                    rMap.Font = rFontS

                    isSel = SL.IsSelected(Idx)

                    If isHighLight Then
                        If isSel Then
                            rMap.TextColor = SMSelTextRGB
                            rMap.BackColor = SMSelBackRGB
                            rSel = rSelS
                        Else
                            rMap.TextColor = SMTextRGB
                            rMap.BackColor = SMBackBlendRGB
                        End If
                    Else
                        If isSel Then
                            rMap.TextColor = SMSelTextRGB
                            rMap.BackColor = SMSelBackRGB
                            rSel = rSelS
                        Else
                            rMap.TextColor = SMTextRGB
                            rMap.BackColor = SMBackRGB
                        End If
                    End If
                    rBack = rBackS

                    If SL.SelectionLength = 0 And SM.wMode = WriteMode.Over And SM.ovrMode.HasFlag(OverModes.Color) Then
                        If FocussedBoxIndex = boxIndex Then
                            If SL.IsOver(Idx, sourceBox.trans, sourceBox.trans) Then
                                If FocussedBoxIndex = boxIndex Then
                                    rMap.TextColor = SMOvrTextRGB
                                    rMap.BackColor = SMOvrBackRGB
                                Else
                                    rMap.TextColor = SMOvrText2RGB
                                    rMap.BackColor = SMOvrBack2RGB
                                End If
                            End If
                        Else
                            If Me.SL.IsOver(Idx, sourceBox.trans, box.trans) Then
                                If FocussedBoxIndex = boxIndex Then
                                    rMap.TextColor = SMOvrTextRGB
                                    rMap.BackColor = SMOvrBackRGB
                                Else
                                    rMap.TextColor = SMOvrText2RGB
                                    rMap.BackColor = SMOvrBack2RGB
                                End If
                            End If
                        End If
                    End If

                    For Each ts In SM.TSS
                        isContains = False
                        If ts.StyleTarget = StyleTarget.All Or ts.StyleTarget = StyleTarget.AllContents Or ts.StyleTarget = StyleTarget.SelectedContents Then
                            Dim continuex As Boolean = False
                            If ts.StyleTarget = StyleTarget.SelectedContents Then
                                continuex = ts.BoxIndex.Contains(boxIndex)
                            Else
                                continuex = True
                            End If
                            If continuex Then
                                If ts.Unit = PointUnit.Byte Then
                                    If ts.Contains(Ix + Iy) Then
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
                                    rMap.TextColor = SMSelTextRGB
                                End If
                                If Not ts.HightlightBackColor.IsEmpty Then
                                    rMap.BackColor = Helper.GetRGB(ts.HightlightBackColor)
                                    rSel = ts.HightlightBackColor
                                ElseIf ts.IsOverride Then
                                    rMap.BackColor = SMSelBackRGB
                                    rSel = rSelS
                                End If
                            Else
                                If Not ts.TextColor.IsEmpty Then
                                    rMap.TextColor = Helper.GetRGB(ts.TextColor)
                                ElseIf ts.IsOverride Then
                                    rMap.TextColor = SMTextRGB
                                End If
                                If Not ts.BackColor.IsEmpty Then
                                    rMap.BackColor = Helper.GetRGB(Helper.Blend(ts.BackColor, rBackS, ts.BackColor.A / 255))
                                ElseIf ts.IsOverride Then
                                    rMap.BackColor = SMBackRGB
                                End If
                            End If

                            If Not ts.BackColor.IsEmpty Then
                                rBack = Helper.Blend(ts.BackColor, rBackS, ts.BackColor.A / 255)
                            ElseIf ts.IsOverride Then
                                rBack = rBackS
                            End If

                            If SL.SelectionLength = 0 And SM.wMode = WriteMode.Over Then
                                If SL.IsCaret(Idx) Then
                                    If FocussedBoxIndex = boxIndex Then
                                        If Not ts.HotTextColor.IsEmpty Then
                                            rMap.TextColor = Helper.GetRGB(ts.HotTextColor)
                                        ElseIf ts.IsOverride Then
                                            rMap.TextColor = SMOvrTextRGB
                                        End If
                                        If Not ts.HotBackColor.IsEmpty Then
                                            rMap.BackColor = Helper.GetRGB(ts.HotBackColor)
                                        ElseIf ts.IsOverride Then
                                            rMap.BackColor = SMOvrBackRGB
                                        End If
                                    Else
                                        If Not ts.HotTextColor2.IsEmpty Then
                                            rMap.TextColor = Helper.GetRGB(ts.HotTextColor2)
                                        ElseIf ts.IsOverride Then
                                            rMap.TextColor = SMOvrText2RGB
                                        End If
                                        If Not ts.HotBackColor2.IsEmpty Then
                                            rMap.BackColor = Helper.GetRGB(ts.HotBackColor2)
                                        ElseIf ts.IsOverride Then
                                            rMap.BackColor = SMOvrBack2RGB
                                        End If
                                    End If
                                End If
                            End If
                            If Not IsNothing(ts.Font) Then
                                rMap.Font = ts.Font
                            ElseIf ts.IsOverride Then
                                rMap.Font = rFontS
                            End If

                        End If
                    Next

                    If isSel Then
                        If Not rSel.IsEmpty Then
                            rMap.BackColor = Helper.GetRGB(Helper.Blend(rSel, rBack, rSel.A / 255))
                        End If
                    End If

                    If (i >= charsLessPerRow And (Ix + Iy) + box.trans.LengthPerData <= QR.Length) Then
                        rMap.TextColor = SMGrayTextRGB
                        rMap.BackColor = SMGrayBackRGB
                        rMap.Font = SM.LessFont
                    ElseIf ((Ix + Iy) < QR.Length And Not (Ix + Iy) + box.trans.LengthPerData <= QR.Length) Then
                        rMap.TextColor = SMGrayTextRGB
                        rMap.BackColor = SMGrayBackRGB
                        rMap.Font = SM.LessFont
                    ElseIf (Ix + Iy) + box.trans.LengthPerData <= QR.Length Then

                    Else
                        rMap.TextColor = SMTextRGB
                        If isHighLight Then
                            rMap.BackColor = SMBackBlendRGB
                        Else
                            rMap.BackColor = SMBackRGB
                        End If
                    End If


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

                If isHighLight Then
                    Helper.FillRectangle(hDC, New Rectangle(rect.X - box.xpad, rect.Y, box.w + box.xpad, SM.row_height), SMBackBlendRGB)
                End If

                Dim rectX As New Rectangle
                Dim paintDefault As Boolean = True
                If SM.usrpaint_content Then
                    rectX.X = rect.X
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
                        rectX.X = rect.X + rX
                        rectX.Y = rect.Y
                        subTx = Mid(Tx, i.Start, i.Length)
                        fd = GetFontData(i.Style.Font)
                        rectX.Width = Helper.GetTextWidth(fd.ABC, subTx)
                        rectX.Height = SM.row_height

                        Dim rY As Single = 0
                        Dim h As Single = 0
                        If Not i.Style.Font Is SM.fnt Then
                            'Dim pos As Long = (vscroll_val + y) * charsPerRow + i.Start
                            ' If SL.IsSelected(pos) Then
                            ' Helper.FillRectangle(hDC, New Rectangle(rect.X + rX, rect.Y, rectX.Width, rectX.Height), SMSelBackRGB)
                            'End If
                            Helper.FillRectangle(hDC, New Rectangle(rect.X + rX - 1, rect.Y, rectX.Width, rectX.Height), i.Style.BackColor)
                            h = Helper.GetTextHeight(fd.hFont)
                            rY = Math.Floor((SM.row_height - h) / 2)
                            If rY < 0 Then rY = 0

                        End If

                        If e.IsVisible(New Rectangle(rectX.X - 1, rectX.Y, rectX.Width, rectX.Height)) Then
                            GDI32.SelectObject(hDC, fd.hFont)
                            GDI32.SetTextColor(hDC, i.Style.TextColor)
                            GDI32.SetBackColor(hDC, i.Style.BackColor)
                            GDI32.TextOut(hDC, rect.X + rX, rect.Y + rY, subTx, subTx.Length)
                        End If

                        If Not h = 0 Then
                            'Helper.DrawRectangle(hDC, New Rectangle(rect.X + rX - 1, rect.Y + rY, rectX.Width - 1, h), Helper.GetRGB(Color.Black))
                        End If
                        rX += rectX.Width
                    Next
                End If

                If Not isIndicator And myFocus Then
                    If boxIndex = 1 Then
                        Dim gg = 4
                    End If
                    If Not (SM.wMode = WriteMode.Over And SL.SelectionLength = 0) Then
                        If indicator_blink Or Not FocussedBoxIndex = boxIndex Then
                            rX = -1
                            Dim px As Integer = 0
                            Dim pxByt As Byte() = Nothing
                            Dim w As Integer = 0
                            Dim h As Integer = 0
                            Dim LFx As Integer = 0
                            Dim sItem As SelectionItem = SL.Curent

                            For i As Integer = 0 To charsPerRow - 1
                                Idx = (e.A + y * SM.col_count) / SM.col_count * charsPerRow + i
                                rMap = rMaps(i)
                                fd = GetFontData(rMap.Font)
                                subTx = Mid(Tx, i + 1, 1)
                                w = Helper.GetTextWidth(fd.ABC, subTx)
                                Dim gobak As Boolean = False
                                If Not FocussedBoxIndex = boxIndex Then
                                    If i = charsPerRow - 1 And Not sItem.LF And SL.IsSelected(Idx) Then
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
                                        px = GDI32.GetPixel(hDC, rect.X + rX + LFx, rect.Y + i2)
                                        pxByt = BitConverter.GetBytes(px)
                                        pxByt(0) = 255 - pxByt(0)
                                        pxByt(1) = 255 - pxByt(1)
                                        pxByt(2) = 255 - pxByt(2)
                                        px = BitConverter.ToInt32(pxByt, 0)
                                        GDI32.SetPixel(hDC, rect.X + rX + LFx, rect.Y + i2, px)
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
                    ElseIf SM.wMode = WriteMode.Over And SL.SelectionLength = 0 And (SM.ovrMode.HasFlag(OverModes.Line) Or FocussedBoxIndex = boxIndex) Then
                        rX = -1
                        Dim px As Integer = 0
                        Dim pxByt As Byte() = Nothing
                        Dim w As Integer = 0
                        Dim h As Integer = 0
                        Dim IsOvered As Boolean = False
                        For i As Integer = 0 To charsPerRow - 1
                            Idx = (e.A + y * SM.col_count) / SM.col_count * charsPerRow + i
                            rMap = rMaps(i)
                            fd = GetFontData(rMap.Font)
                            w = Helper.GetTextWidth(fd.ABC, Mid(Tx, i + 1, 1))
                            If FocussedBoxIndex = boxIndex Then
                                If indicator_blink Then
                                    If SL.IsCaret(Idx) Then
                                        h = SM.row_height ' Helper.GetTextHeight(fd.hFont)
                                        For i2 As Integer = 0 To w - 1
                                            px = GDI32.GetPixel(hDC, rect.X + rX + i2, rect.Y + h - 1)
                                            pxByt = BitConverter.GetBytes(px)
                                            pxByt(0) = 255 - pxByt(0)
                                            pxByt(1) = 255 - pxByt(1)
                                            pxByt(2) = 255 - pxByt(2)
                                            px = BitConverter.ToInt32(pxByt, 0)
                                            GDI32.SetPixel(hDC, rect.X + rX + i2, rect.Y + h - 1, px)
                                        Next
                                        isIndicator = True
                                        Exit For
                                    End If
                                End If
                            Else
                                If Me.SL.IsOver(Idx, sourceBox.trans, box.trans) Then
                                    h = SM.row_height ' Helper.GetTextHeight(fd.hFont)
                                    For i2 As Integer = 0 To w - 1
                                        px = GDI32.GetPixel(hDC, rect.X + rX + i2, rect.Y + h - 1)
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
                                        GDI32.SetPixel(hDC, rect.X + rX + i2, rect.Y + h - 1, px)
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

                rect.Y += SM.row_height
                For i As Integer = 0 To rMaps.Length - 1
                    rMaps(i).Reset()
                Next

                rGroups.Clear()
            Next

            rect.X += box.w
        Next
        GDI32.RestoreDC(hDC, hState)
        GDI32.DeleteObject(hRgn)
    End Sub
    Friend Sub RenderBorder(e As RenderParam)
        Dim width As Integer = SM.borderW
        Dim rect As New Rectangle(SM.all_xpad, SM.all_ypad, e.WContent + width, MyRect.Height)

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

        Dim hBrushPrevious As IntPtr = GDI32.SelectObject(e.hDC, hBrush)
        Dim hPenPrevious As IntPtr = GDI32.SelectObject(e.hDC, hPen)

        If Not SM.header_mode = HeaderMode.Hidden Then
            GDI32.Rectangle(hDC, rect.X, rect.Y, rect.Right, rect.Y + width)
            Dim hh As Integer = 0
            If Not SM.header_mode = HeaderMode.Hidden Then
                hh = SM.header_height
                GDI32.Rectangle(hDC, rect.X, rect.Y + hh, rect.Right, rect.Y + hh + width)
            End If
            GDI32.Rectangle(hDC, rect.X, rect.Bottom - width, rect.Right, rect.Bottom)
        End If

        GDI32.Rectangle(hDC, rect.X, rect.Y, rect.X + width, rect.Bottom)

        If Not SM.offset_mode = OffsetMode.Hidden Then
            rect.X += SM.offsetBox_width
        End If

        Dim sourceBox As BoxItem = SM.Box(FocussedBoxIndex)
        Dim box As BoxItem = Nothing
        For boxIndex As Integer = 0 To SM.Box.Count
            If Not boxIndex = SM.Box.Count Then
                box = SM.Box(boxIndex)
            End If

            GDI32.Rectangle(hDC, rect.X, rect.Y, rect.X + width, rect.Bottom)

            rect.X += box.w + box.xpad
        Next
        GDI32.SelectObject(e.hDC, hBrushPrevious)
        GDI32.SelectObject(e.hDC, hPenPrevious)

        GDI32.DeleteObject(hBrush)
        GDI32.RestoreDC(hDC, hState)
        GDI32.DeleteObject(hRgn)
    End Sub
    Friend Function GetCharsPerRow(box As BoxItem) As Integer
        Dim perData As Integer = box.trans.CharsPerData + box.trans.Sparator
        Dim colFloor As Integer = Math.Floor(SM.col_count / box.trans.LengthPerData) * box.trans.LengthPerData
        Dim charsPerRow As Integer = (colFloor / box.trans.LengthPerData * perData)
        Dim charsLessPerRow As Integer = (colFloor / box.trans.LengthPerData * perData)
        Dim perLess As Integer = (SM.col_count - colFloor) * 2
        If (SM.col_count - colFloor) > 0 Then perLess += box.trans.Sparator
        charsPerRow += perLess
        Return charsPerRow
    End Function
    Friend Shared Function GetFontData(ByVal Font As Font) As FontData
        Return StyleSet.GetFontData(Font)
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
        Dim rect As New Rectangle(SM.all_xpad, SM.all_ypad, MyRect.Width, MyRect.Height)
        If Not SM.offset_mode = OffsetMode.Hidden Then
            rect.X += SM.offsetBox_width
        End If

        If rect.Height <= 0 Then Exit Sub
        rect.X -= hscroll_val

        Dim fd As FontData = Nothing
        If Not SM.header_mode = HeaderMode.Hidden Then
            rect.Y += SM.header_height + SM.content_ypad
        End If

        rect.Height = SM.row_height

        Dim bY As Integer = rect.Y

        Dim SMBackRGB As Integer ' = Helper.GetRGB(SM.bkClr)
        Dim SMTextRGB As Integer ' = Helper.GetRGB(Helper.Blend(SM.txClr, SM.bkClr, SM.txClr.A / 255))
        Dim SMSelTextRGB As Integer ' = Helper.GetRGB(Helper.Blend(SM.seltxClr, SM.bkClr, SM.seltxClr.A / 255))
        Dim SMSelBackRGB As Integer ' = Helper.GetRGB(Helper.Blend(SM.selbkClr, SM.bkClr, SM.selbkClr.A / 255))
        Dim SMOvrTextRGB As Integer ' = Helper.GetRGB(Helper.Blend(SM.ovrtxClr, SM.bkClr, SM.ovrtxClr.A / 255))
        Dim SMOvrBackRGB As Integer '= Helper.GetRGB(Helper.Blend(SM.ovrbkClr, SM.bkClr, SM.ovrbkClr.A / 255))
        Dim SMOvrText2RGB As Integer ' = Helper.GetRGB(Helper.Blend(SM.ovrtx2Clr, SM.bkClr, SM.ovrtx2Clr.A / 255))
        Dim SMOvrBack2RGB As Integer ' = Helper.GetRGB(Helper.Blend(SM.ovrbk2Clr, SM.bkClr, SM.ovrbk2Clr.A / 255))
        Dim SMGrayTextRGB As Integer ' = Helper.GetRGB(Helper.Blend(SM.GrayTextColor, SM.bkClr, SM.GrayTextColor.A / 255))
        Dim SMBackBlendRGB As Integer '= Helper.GetRGB(Helper.Blend(SM.highLightLineColor, SM.bkClr, SM.highLightLineColor.A / 255))

        Dim sourceBox As BoxItem = SM.Box(FocussedBoxIndex)
        Dim sItem2 As SelectionItem = Me.SL.Curent
        Dim caretz As Long = sItem2.car
        If sItem2.LF Then caretz -= 1
        Dim line As Long = Math.Floor(caretz / GetCharsPerRow(sourceBox))
        Dim index As Long = line * SM.col_count
        Dim isHighLight As Boolean = False
        Dim rBackS As Color = Nothing
        Dim rSelS As Color = Nothing
        Dim rFontS As Font = Nothing
        For boxIndex As Integer = 0 To SM.Box.Count - 1
            Dim box As BoxItem = SM.Box(boxIndex)
            rect.X += box.xpad
            'If boxIndex = SelectedBoxIndex Then
            Dim SL As SelectionManager = Nothing
            If sourceBox Is box Then
                SL = Me.SL
            Else
                SL = Me.SL.TransformSelection(sourceBox.trans, box.trans)
            End If
            If IsNothing(box.Style) Then
                SMBackRGB = Helper.GetRGB(SM.bkClr)
                SMTextRGB = Helper.GetRGB(Helper.Blend(SM.txClr, SM.bkClr, SM.txClr.A / 255))
                SMSelTextRGB = Helper.GetRGB(Helper.Blend(SM.seltxClr, SM.bkClr, SM.seltxClr.A / 255))
                SMSelBackRGB = Helper.GetRGB(Helper.Blend(SM.selbkClr, SM.bkClr, SM.selbkClr.A / 255))
                SMOvrTextRGB = Helper.GetRGB(Helper.Blend(SM.ovrtxClr, SM.bkClr, SM.ovrtxClr.A / 255))
                SMOvrBackRGB = Helper.GetRGB(Helper.Blend(SM.ovrbkClr, SM.bkClr, SM.ovrbkClr.A / 255))
                SMOvrText2RGB = Helper.GetRGB(Helper.Blend(SM.ovrtx2Clr, SM.bkClr, SM.ovrtx2Clr.A / 255))
                SMOvrBack2RGB = Helper.GetRGB(Helper.Blend(SM.ovrbk2Clr, SM.bkClr, SM.ovrbk2Clr.A / 255))
                SMGrayTextRGB = Helper.GetRGB(Helper.Blend(SM.graytxClr, SM.bkClr, SM.graytxClr.A / 255))
                SMBackBlendRGB = Helper.GetRGB(Helper.Blend(SM.highLightLineColor, SM.bkClr, SM.highLightLineColor.A / 255))
                rBackS = SM.bkClr
                rSelS = SM.selbkClr
                rFontS = SM.Font
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
                rFontS = sts.Font
                SMBackRGB = Helper.GetRGB(sts.BackColor)
                SMTextRGB = Helper.GetRGB(Helper.Blend(sts.TextColor, sts.BackColor, sts.TextColor.A / 255))
                SMSelTextRGB = Helper.GetRGB(Helper.Blend(sts.HightlightTextColor, sts.BackColor, sts.HightlightTextColor.A / 255))
                SMSelBackRGB = Helper.GetRGB(Helper.Blend(sts.HightlightBackColor, sts.BackColor, sts.HightlightBackColor.A / 255))
                SMOvrTextRGB = Helper.GetRGB(Helper.Blend(sts.HotTextColor, sts.BackColor, sts.HotTextColor.A / 255))
                SMOvrBackRGB = Helper.GetRGB(Helper.Blend(sts.HotBackColor, sts.BackColor, sts.HotBackColor.A / 255))
                SMOvrText2RGB = Helper.GetRGB(Helper.Blend(sts.HotTextColor2, sts.BackColor, sts.HotTextColor2.A / 255))
                SMOvrBack2RGB = Helper.GetRGB(Helper.Blend(sts.HotBackColor2, sts.BackColor, sts.HotBackColor2.A / 255))
                SMGrayTextRGB = Helper.GetRGB(Helper.Blend(sts.GrayTextColor, sts.BackColor, sts.GrayTextColor.A / 255))
                SMBackBlendRGB = Helper.GetRGB(Helper.Blend(SM.highLightLineColor, sts.BackColor, SM.highLightLineColor.A / 255))
                rBackS = sts.BackColor
                rSelS = sts.HightlightBackColor
            End If
            Dim N As Long = 0
            Dim Ix As Integer = 0
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

                Tx = ""
                Iy = 0
                For i As Integer = 0 To SM.col_count - 1 Step box.trans.LengthPerData
                    If i >= colFloor Then
                        For i2 As Integer = colFloor To SM.col_count - 1
                            If (Ix + i) < QR.Length Then
                                Tx &= Helper.Hex2(QR.Buffer(Ix + i2))
                            Else
                                Tx &= "".PadRight(2)
                            End If
                        Next
                        Tx &= "".PadRight(box.trans.Sparator)
                    Else
                        If (Ix + i) < QR.Length And (Ix + i) + box.trans.LengthPerData <= QR.Length Then
                            Tx &= box.trans.GetString(QR.Buffer, Ix + i).PadRight(perData)
                        ElseIf (Ix + i) < QR.Length Then
                            For i2 As Integer = 0 To (Ix + i) + box.trans.LengthPerData - QR.Length - 1
                                If (Ix + i) < QR.Length Then
                                    Tx &= Helper.Hex2(QR.Buffer(Ix + i + i2)).PadRight(box.trans.CharsPerData)
                                Else
                                    Tx &= "".PadRight(2)
                                End If
                            Next
                            Tx &= "".PadRight(box.trans.Sparator)
                        Else
                            Tx &= "".PadRight(perData)
                        End If
                    End If
                    Iy += perData
                Next

                Iy = 0
                Dim iZ As Integer = 0
                For i As Integer = 0 To charsPerRow - 1
                    Idx = (A + y * SM.col_count) / SM.col_count * charsPerRow + i

                    rMap = rMaps(i)
                    rMap.Font = rFontS

                    isSel = SL.IsSelected(Idx)
                    If isHighLight Then
                        If isSel Then
                            rMap.TextColor = SMSelTextRGB
                            rMap.BackColor = SMSelBackRGB
                            rSel = rSelS
                        Else
                            rMap.TextColor = SMTextRGB
                            rMap.BackColor = SMBackBlendRGB
                        End If
                    Else
                        If isSel Then
                            rMap.TextColor = SMSelTextRGB
                            rMap.BackColor = SMSelBackRGB
                            rSel = rSelS
                        Else
                            rMap.TextColor = SMTextRGB
                            rMap.BackColor = SMBackRGB
                        End If
                    End If
                    rBack = rBackS

                    If SL.SelectionLength = 0 And SM.wMode = WriteMode.Over And SM.ovrMode.HasFlag(OverModes.Color) Then
                        If FocussedBoxIndex = boxIndex Then
                            If SL.IsOver(Idx, sourceBox.trans, sourceBox.trans) Then
                                If FocussedBoxIndex = boxIndex Then
                                    rMap.TextColor = SMOvrTextRGB
                                    rMap.BackColor = SMOvrBackRGB
                                Else
                                    rMap.TextColor = SMOvrText2RGB
                                    rMap.BackColor = SMOvrBack2RGB
                                End If
                            End If
                        Else
                            If Me.SL.IsOver(Idx, sourceBox.trans, box.trans) Then
                                If FocussedBoxIndex = boxIndex Then
                                    rMap.TextColor = SMOvrTextRGB
                                    rMap.BackColor = SMOvrBackRGB
                                Else
                                    rMap.TextColor = SMOvrText2RGB
                                    rMap.BackColor = SMOvrBack2RGB
                                End If
                            End If
                        End If
                    End If

                    For Each ts In SM.TSS
                        isContains = False
                        If ts.StyleTarget = StyleTarget.All Or ts.StyleTarget = StyleTarget.AllContents Or ts.StyleTarget = StyleTarget.SelectedContents Then
                            Dim continuex As Boolean = False
                            If ts.StyleTarget = StyleTarget.SelectedContents Then
                                continuex = ts.BoxIndex.Contains(boxIndex)
                            Else
                                continuex = True
                            End If
                            If continuex Then
                                If ts.Unit = PointUnit.Byte Then
                                    If ts.Contains(Ix + Iy) Then
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
                                    rMap.TextColor = SMSelTextRGB
                                End If
                                If Not ts.HightlightBackColor.IsEmpty Then
                                    rMap.BackColor = Helper.GetRGB(ts.HightlightBackColor)
                                    rSel = ts.HightlightBackColor
                                ElseIf ts.IsOverride Then
                                    rMap.BackColor = SMSelBackRGB
                                    rSel = rSelS
                                End If
                            Else
                                If Not ts.TextColor.IsEmpty Then
                                    rMap.TextColor = Helper.GetRGB(ts.TextColor)
                                ElseIf ts.IsOverride Then
                                    rMap.TextColor = SMTextRGB
                                End If
                                If Not ts.BackColor.IsEmpty Then
                                    rMap.BackColor = Helper.GetRGB(Helper.Blend(ts.BackColor, rBackS, ts.BackColor.A / 255))
                                ElseIf ts.IsOverride Then
                                    rMap.BackColor = SMBackRGB
                                End If
                            End If

                            If Not ts.BackColor.IsEmpty Then
                                rBack = Helper.Blend(ts.BackColor, rBackS, ts.BackColor.A / 255)
                            ElseIf ts.IsOverride Then
                                rBack = rBackS
                            End If

                            If SL.SelectionLength = 0 And SM.wMode = WriteMode.Over Then
                                If SL.IsCaret(Idx) Then
                                    If FocussedBoxIndex = boxIndex Then
                                        If Not ts.HotTextColor.IsEmpty Then
                                            rMap.TextColor = Helper.GetRGB(ts.HotTextColor)
                                        ElseIf ts.IsOverride Then
                                            rMap.TextColor = SMOvrTextRGB
                                        End If
                                        If Not ts.HotBackColor.IsEmpty Then
                                            rMap.BackColor = Helper.GetRGB(ts.HotBackColor)
                                        ElseIf ts.IsOverride Then
                                            rMap.BackColor = SMOvrBackRGB
                                        End If
                                    Else
                                        If Not ts.HotTextColor2.IsEmpty Then
                                            rMap.TextColor = Helper.GetRGB(ts.HotTextColor2)
                                        ElseIf ts.IsOverride Then
                                            rMap.TextColor = SMOvrText2RGB
                                        End If
                                        If Not ts.HotBackColor2.IsEmpty Then
                                            rMap.BackColor = Helper.GetRGB(ts.HotBackColor2)
                                        ElseIf ts.IsOverride Then
                                            rMap.BackColor = SMOvrBack2RGB
                                        End If
                                    End If
                                End If
                            End If
                            If Not IsNothing(ts.Font) Then
                                rMap.Font = ts.Font
                            ElseIf ts.IsOverride Then
                                rMap.Font = rFontS
                            End If

                        End If
                    Next

                    If isSel Then
                        If Not rSel.IsEmpty Then
                            rMap.BackColor = Helper.GetRGB(Helper.Blend(rSel, rBack, rSel.A / 255))
                        End If
                    End If

                    If (i >= charsLessPerRow And (Ix + Iy) + box.trans.LengthPerData <= QR.Length) Then
                        rMap.TextColor = SMGrayTextRGB
                        rMap.Font = SM.LessFont
                    ElseIf ((Ix + Iy) < QR.Length And Not (Ix + Iy) + box.trans.LengthPerData <= QR.Length) Then
                        rMap.TextColor = SMGrayTextRGB
                        rMap.Font = SM.LessFont
                    ElseIf (Ix + Iy) + box.trans.LengthPerData < QR.Length Then

                    Else
                        rMap.TextColor = SMTextRGB
                        rMap.BackColor = SMBackRGB
                    End If


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

                If boxIndex = 1 Then
                    Dim hh = 4
                End If
                If FocussedBoxIndex = boxIndex And Not (SM.wMode = WriteMode.Over And SL.SelectionLength = 0) Then
                    rX = -1
                    Dim px As Integer = 0
                    Dim pxByt As Byte() = Nothing
                    Dim w As Integer = 0
                    Dim LFx As Integer = 0
                    Dim sItem As SelectionItem = SL.Curent
                    For i As Integer = 0 To charsPerRow - 1
                        Idx = (A + y * SM.col_count) / SM.col_count * charsPerRow + i
                        rMap = rMaps(i)
                        fd = GetFontData(rMap.Font)
                        w = Helper.GetTextWidth(fd.ABC, Mid(Tx, i + 1, 1))
                        If SL.IsCaret(Idx) Then
                            If sItem.LF Then
                                LFx = w + 1
                            Else
                                LFx = 0
                            End If
                            Me.Invalidate(New Rectangle(rect.X + rX + LFx, rect.Y, 1, SM.row_height))
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
                    For i As Integer = 0 To charsPerRow - 1
                        Idx = (A + y * SM.col_count) / SM.col_count * charsPerRow + i
                        rMap = rMaps(i)
                        fd = GetFontData(rMap.Font)
                        w = Helper.GetTextWidth(fd.ABC, Mid(Tx, i + 1, 1))
                        If FocussedBoxIndex = boxIndex Then
                            If SL.IsCaret(Idx) Then
                                h = SM.row_height ' Helper.GetTextHeight(fd.hFont)
                                Me.Invalidate(New Rectangle(rect.X + rX, rect.Y, w, h))
                                found = True
                                Exit For
                            End If
                        Else
                            If Me.SL.IsOver(Idx, sourceBox.trans, box.trans) Then
                                h = SM.row_height ' Helper.GetTextHeight(fd.hFont)
                                Me.Invalidate(New Rectangle(rect.X + rX, rect.Y, w, h))
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
                For i As Integer = 0 To rMaps.Length - 1
                    rMaps(i).Reset()
                Next

                rGroups.Clear()

                If found Then
                    Exit For
                End If
            Next
            'End If
            rect.X += box.w
        Next
    End Sub

#End Region

#Region "Mouse Event Handler"
    Friend isDown As Boolean = False
    Friend m As Point
    Friend isMove As Boolean = False
    Friend LCtrlEnabled As Boolean = False
    Friend dontSnap As Boolean = False
    Friend multisel As Boolean = True
    Friend fullRowSelection As Boolean = False
    Friend dbSnap As Boolean = False
    Friend revCur As Cursor = Helper.FlipCursor
    Friend curentCur As Integer = 1
    Friend selPower As Boolean = False
    Private Sub TransformBox_MouseDown(sender As Object, e As MouseEventArgs) Handles MyBase.MouseDown
        m = e.Location

        If e.Button = MouseButtons.Left Then
            isDown = True
            isMove = False
            dbSnap = False
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
            MouseDownEvent()
            If Not dbSnap Then CursorChanger()
            InvalidateCollumnHeader()
            Me.Update()
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
            MouseMoveEvent()

            If SM.AutoSnap Then
                If Not SL.SelectionLength = 0 And Not isMove Then
                    Dim sItem As SelectionItem = SL.Curent
                    Dim bakItem As SelectionItem = sItem.Clone
                    If sItem.anc < sItem.car Then
                        Dim box As BoxItem = SM.Box(FocussedBoxIndex)
                        Dim charsPerRow As Integer = GetCharsPerRow(box)
                        Dim col As Long = sItem.anc Mod charsPerRow
                        Dim perData As Integer = (box.trans.CharsPerData + box.trans.Sparator)
                        Dim colmod As Integer = col Mod perData

                        Dim Ix As Long = Math.Floor(sItem.anc / charsPerRow) * SM.col_count
                        Ix += Math.Floor(col / perData) * box.trans.LengthPerData
                        Ix -= QR.Position
                        Dim data As String = box.trans.GetString(QR.Buffer, Ix)
                        Dim chrLen As Integer = data.Length 'box.Transformer.CharsPerData

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

                    InvalidateSEL(Me.SL, item)
                    InvalidateSEL(SL, sItem)
                    isMove = True
                End If
            End If

            InvalidateCollumnHeader()
            Me.Update()
        Else
            CursorChanger()
        End If
    End Sub
    Private Sub TransformBox_MouseUp(sender As Object, e As MouseEventArgs) Handles MyBase.MouseUp
        If isDown Then
            isDown = False
            dbSnap = False
            fullRowSelection = False
            If Not SL.SelectionLength = 0 Then
                'dontSnap = False
                Dim rl As Integer = QR.Update(vscroll_val * SM.col_count, ShowedLength)
                If SM.AutoSnap Then
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

                    Dim evt As New HexBoxSelectionEventArgs(FocussedBoxIndex, bakSL, FocussedBoxIndex, Me.SL)
                    RaiseEvent SelectionChanged(Me, evt)
                    If evt.Cancel Then
                        Me.SL = evt.OldSelection
                        InvalidateSEL(evt.OldSelection)
                        InvalidateSEL(evt.NewSelection)
                    End If

                    InvalidateSEL(Me.SL, item)
                    InvalidateSEL(SL, SL.Curent)
                End If
                InvalidateCollumnHeader()
                Me.Update()
            End If
        End If
    End Sub
    Friend Sub MouseDownEvent()
        Dim A As Long = vscroll_val * SM.col_count
        Dim rect As New Rectangle(SM.all_xpad, SM.all_ypad, MyRect.Width, MyRect.Height)
        If Not SM.offset_mode = OffsetMode.Hidden Then
            rect.X += SM.offsetBox_width
        End If

        If rect.Height <= 0 Then Exit Sub
        rect.X -= hscroll_val

        Dim fd As FontData = Nothing
        If Not SM.header_mode = HeaderMode.Hidden Then
            rect.Y += SM.header_height + SM.content_ypad
        End If

        rect.Height = SM.row_height

        Dim bY As Integer = rect.Y

        Dim ShowedRowCount As Integer = Me.ShowedRowCount
        Dim remainData As Long = bb.GetLength - A
        Dim remainRow As Long = Math.Ceiling(remainData / SM.col_count)
        Dim LastRowLength As Integer = SM.col_count
        If ShowedRowCount >= remainRow Then
            ShowedRowCount = remainRow
            LastRowLength = remainData Mod SM.col_count
            If LastRowLength = 0 Then LastRowLength = SM.col_count
        End If

        Dim boxHeight As Integer = SM.row_height * ShowedRowCount

        If Not SM.header_mode = HeaderMode.Hidden Then
            boxHeight += SM.header_height + SM.content_ypad
        End If
        Dim m As Point = Me.m
        If m.X < rect.X Then
            m.X = rect.X
            fullRowSelection = True
        End If
        Dim boxWidth As Integer = 0
        For Each i In SM.Box
            boxWidth += i.w + i.xpad
        Next
        If m.X > rect.X + boxWidth Then
            m.X = rect.X + boxWidth
        End If
        Dim fullColSelection As Boolean = False
        If m.Y < rect.Y Then
            If Not SM.header_mode = HeaderMode.Hidden Then
                dbSnap = True
            End If
            m.Y = rect.Y
            fullColSelection = True
        End If
        If m.Y > boxHeight Then
            m.Y = boxHeight
        End If
        Dim SMBackRGB As Integer ' = Helper.GetRGB(SM.bkClr)
        Dim SMTextRGB As Integer ' = Helper.GetRGB(Helper.Blend(SM.txClr, SM.bkClr, SM.txClr.A / 255))
        Dim SMSelTextRGB As Integer ' = Helper.GetRGB(Helper.Blend(SM.seltxClr, SM.bkClr, SM.seltxClr.A / 255))
        Dim SMSelBackRGB As Integer ' = Helper.GetRGB(Helper.Blend(SM.selbkClr, SM.bkClr, SM.selbkClr.A / 255))
        Dim SMOvrTextRGB As Integer ' = Helper.GetRGB(Helper.Blend(SM.ovrtxClr, SM.bkClr, SM.ovrtxClr.A / 255))
        Dim SMOvrBackRGB As Integer '= Helper.GetRGB(Helper.Blend(SM.ovrbkClr, SM.bkClr, SM.ovrbkClr.A / 255))
        Dim SMOvrText2RGB As Integer ' = Helper.GetRGB(Helper.Blend(SM.ovrtx2Clr, SM.bkClr, SM.ovrtx2Clr.A / 255))
        Dim SMOvrBack2RGB As Integer ' = Helper.GetRGB(Helper.Blend(SM.ovrbk2Clr, SM.bkClr, SM.ovrbk2Clr.A / 255))
        Dim SMGrayTextRGB As Integer ' = Helper.GetRGB(Helper.Blend(SM.GrayTextColor, SM.bkClr, SM.GrayTextColor.A / 255))
        Dim SMBackBlendRGB As Integer '= Helper.GetRGB(Helper.Blend(SM.highLightLineColor, SM.bkClr, SM.highLightLineColor.A / 255))

        Dim sourceBox As BoxItem = SM.Box(FocussedBoxIndex)
        Dim sItem2 As SelectionItem = Me.SL.Curent
        Dim caretz As Long = sItem2.car
        If sItem2.LF Then caretz -= 1
        Dim line As Long = Math.Floor(caretz / GetCharsPerRow(sourceBox))
        Dim index As Long = line * SM.col_count
        Dim isHighLight As Boolean = False
        Dim rBackS As Color = Nothing
        Dim rSelS As Color = Nothing
        Dim rFontS As Font = Nothing
        For boxIndex As Integer = 0 To SM.Box.Count - 1
            Dim box As BoxItem = SM.Box(boxIndex)
            rect.X += box.xpad

            Dim SL As SelectionManager = Nothing
            If sourceBox Is box Then
                SL = Me.SL
            Else
                SL = Me.SL.TransformSelection(sourceBox.trans, box.trans)
            End If
            If IsNothing(box.Style) Then
                SMBackRGB = Helper.GetRGB(SM.bkClr)
                SMTextRGB = Helper.GetRGB(Helper.Blend(SM.txClr, SM.bkClr, SM.txClr.A / 255))
                SMSelTextRGB = Helper.GetRGB(Helper.Blend(SM.seltxClr, SM.bkClr, SM.seltxClr.A / 255))
                SMSelBackRGB = Helper.GetRGB(Helper.Blend(SM.selbkClr, SM.bkClr, SM.selbkClr.A / 255))
                SMOvrTextRGB = Helper.GetRGB(Helper.Blend(SM.ovrtxClr, SM.bkClr, SM.ovrtxClr.A / 255))
                SMOvrBackRGB = Helper.GetRGB(Helper.Blend(SM.ovrbkClr, SM.bkClr, SM.ovrbkClr.A / 255))
                SMOvrText2RGB = Helper.GetRGB(Helper.Blend(SM.ovrtx2Clr, SM.bkClr, SM.ovrtx2Clr.A / 255))
                SMOvrBack2RGB = Helper.GetRGB(Helper.Blend(SM.ovrbk2Clr, SM.bkClr, SM.ovrbk2Clr.A / 255))
                SMGrayTextRGB = Helper.GetRGB(Helper.Blend(SM.graytxClr, SM.bkClr, SM.graytxClr.A / 255))
                SMBackBlendRGB = Helper.GetRGB(Helper.Blend(SM.highLightLineColor, SM.bkClr, SM.highLightLineColor.A / 255))
                rBackS = SM.bkClr
                rSelS = SM.selbkClr
                rFontS = SM.Font
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
                rFontS = sts.Font
                SMBackRGB = Helper.GetRGB(sts.BackColor)
                SMTextRGB = Helper.GetRGB(Helper.Blend(sts.TextColor, sts.BackColor, sts.TextColor.A / 255))
                SMSelTextRGB = Helper.GetRGB(Helper.Blend(sts.HightlightTextColor, sts.BackColor, sts.HightlightTextColor.A / 255))
                SMSelBackRGB = Helper.GetRGB(Helper.Blend(sts.HightlightBackColor, sts.BackColor, sts.HightlightBackColor.A / 255))
                SMOvrTextRGB = Helper.GetRGB(Helper.Blend(sts.HotTextColor, sts.BackColor, sts.HotTextColor.A / 255))
                SMOvrBackRGB = Helper.GetRGB(Helper.Blend(sts.HotBackColor, sts.BackColor, sts.HotBackColor.A / 255))
                SMOvrText2RGB = Helper.GetRGB(Helper.Blend(sts.HotTextColor2, sts.BackColor, sts.HotTextColor2.A / 255))
                SMOvrBack2RGB = Helper.GetRGB(Helper.Blend(sts.HotBackColor2, sts.BackColor, sts.HotBackColor2.A / 255))
                SMGrayTextRGB = Helper.GetRGB(Helper.Blend(sts.GrayTextColor, sts.BackColor, sts.GrayTextColor.A / 255))
                SMBackBlendRGB = Helper.GetRGB(Helper.Blend(SM.highLightLineColor, sts.BackColor, SM.highLightLineColor.A / 255))
                rBackS = sts.BackColor
                rSelS = sts.HightlightBackColor
            End If
            Dim N As Long = 0
            Dim Ix As Integer = 0
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

            Dim isSel As Boolean = False
            Dim isContains As Boolean = False
            Dim rBack As Color = Nothing
            Dim rSel As Color = Nothing
            For y As Integer = 0 To ShowedRowCount - 1
                Ix = (A - QR.Position) + y * SM.col_count

                N = (vscroll_val + y) * SM.col_count
                isHighLight = N <= index And index < (N + SM.col_count)
                isHighLight = isHighLight And SM.highlightLine

                Tx = ""
                Iy = 0
                For i As Integer = 0 To SM.col_count - 1 Step box.trans.LengthPerData
                    If i >= colFloor Then
                        For i2 As Integer = colFloor To SM.col_count - 1
                            If (Ix + i) < QR.Length Then
                                Tx &= Helper.Hex2(QR.Buffer(Ix + i2))
                            Else
                                Tx &= "".PadRight(2)
                            End If
                        Next
                        Tx &= "".PadRight(box.trans.Sparator)
                    Else
                        If (Ix + i) < QR.Length And (Ix + i) + box.trans.LengthPerData <= QR.Length Then
                            Tx &= box.trans.GetString(QR.Buffer, Ix + i).PadRight(perData)
                        ElseIf (Ix + i) < QR.Length Then
                            For i2 As Integer = 0 To (Ix + i) + box.trans.LengthPerData - QR.Length - 1
                                If (Ix + i) < QR.Length Then
                                    Tx &= Helper.Hex2(QR.Buffer(Ix + i + i2)).PadRight(box.trans.CharsPerData)
                                Else
                                    Tx &= "".PadRight(2)
                                End If
                            Next
                            Tx &= "".PadRight(box.trans.Sparator)
                        Else
                            Tx &= "".PadRight(perData)
                        End If
                    End If
                    Iy += perData
                Next

                Iy = 0
                Dim iZ As Integer = 0
                For i As Integer = 0 To charsPerRow - 1
                    Idx = (A + y * SM.col_count) / SM.col_count * charsPerRow + i

                    rMap = rMaps(i)
                    rMap.Font = rFontS

                    isSel = SL.IsSelected(Idx)
                    If isHighLight Then
                        If isSel Then
                            rMap.TextColor = SMSelTextRGB
                            rMap.BackColor = SMSelBackRGB
                            rSel = rSelS
                        Else
                            rMap.TextColor = SMTextRGB
                            rMap.BackColor = SMBackBlendRGB
                        End If
                    Else
                        If isSel Then
                            rMap.TextColor = SMSelTextRGB
                            rMap.BackColor = SMSelBackRGB
                            rSel = rSelS
                        Else
                            rMap.TextColor = SMTextRGB
                            rMap.BackColor = SMBackRGB
                        End If
                    End If
                    rBack = rBackS

                    If SL.SelectionLength = 0 And SM.wMode = WriteMode.Over And SM.ovrMode.HasFlag(OverModes.Color) Then
                        If FocussedBoxIndex = boxIndex Then
                            If SL.IsOver(Idx, sourceBox.trans, sourceBox.trans) Then
                                If FocussedBoxIndex = boxIndex Then
                                    rMap.TextColor = SMOvrTextRGB
                                    rMap.BackColor = SMOvrBackRGB
                                Else
                                    rMap.TextColor = SMOvrText2RGB
                                    rMap.BackColor = SMOvrBack2RGB
                                End If
                            End If
                        Else
                            If Me.SL.IsOver(Idx, sourceBox.trans, box.trans) Then
                                If FocussedBoxIndex = boxIndex Then
                                    rMap.TextColor = SMOvrTextRGB
                                    rMap.BackColor = SMOvrBackRGB
                                Else
                                    rMap.TextColor = SMOvrText2RGB
                                    rMap.BackColor = SMOvrBack2RGB
                                End If
                            End If
                        End If
                    End If

                    For Each ts In SM.TSS
                        isContains = False
                        If ts.StyleTarget = StyleTarget.All Or ts.StyleTarget = StyleTarget.AllContents Or ts.StyleTarget = StyleTarget.SelectedContents Then
                            Dim continuex As Boolean = False
                            If ts.StyleTarget = StyleTarget.SelectedContents Then
                                continuex = ts.BoxIndex.Contains(boxIndex)
                            Else
                                continuex = True
                            End If
                            If continuex Then
                                If ts.Unit = PointUnit.Byte Then
                                    If ts.Contains(Ix + Iy) Then
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
                                    rMap.TextColor = SMSelTextRGB
                                End If
                                If Not ts.HightlightBackColor.IsEmpty Then
                                    rMap.BackColor = Helper.GetRGB(ts.HightlightBackColor)
                                    rSel = ts.HightlightBackColor
                                ElseIf ts.IsOverride Then
                                    rMap.BackColor = SMSelBackRGB
                                    rSel = rSelS
                                End If
                            Else
                                If Not ts.TextColor.IsEmpty Then
                                    rMap.TextColor = Helper.GetRGB(ts.TextColor)
                                ElseIf ts.IsOverride Then
                                    rMap.TextColor = SMTextRGB
                                End If
                                If Not ts.BackColor.IsEmpty Then
                                    rMap.BackColor = Helper.GetRGB(Helper.Blend(ts.BackColor, rBackS, ts.BackColor.A / 255))
                                ElseIf ts.IsOverride Then
                                    rMap.BackColor = SMBackRGB
                                End If
                            End If

                            If Not ts.BackColor.IsEmpty Then
                                rBack = Helper.Blend(ts.BackColor, rBackS, ts.BackColor.A / 255)
                            ElseIf ts.IsOverride Then
                                rBack = rBackS
                            End If

                            If SL.SelectionLength = 0 And SM.wMode = WriteMode.Over Then
                                If SL.IsCaret(Idx) Then
                                    If FocussedBoxIndex = boxIndex Then
                                        If Not ts.HotTextColor.IsEmpty Then
                                            rMap.TextColor = Helper.GetRGB(ts.HotTextColor)
                                        ElseIf ts.IsOverride Then
                                            rMap.TextColor = SMOvrTextRGB
                                        End If
                                        If Not ts.HotBackColor.IsEmpty Then
                                            rMap.BackColor = Helper.GetRGB(ts.HotBackColor)
                                        ElseIf ts.IsOverride Then
                                            rMap.BackColor = SMOvrBackRGB
                                        End If
                                    Else
                                        If Not ts.HotTextColor2.IsEmpty Then
                                            rMap.TextColor = Helper.GetRGB(ts.HotTextColor2)
                                        ElseIf ts.IsOverride Then
                                            rMap.TextColor = SMOvrText2RGB
                                        End If
                                        If Not ts.HotBackColor2.IsEmpty Then
                                            rMap.BackColor = Helper.GetRGB(ts.HotBackColor2)
                                        ElseIf ts.IsOverride Then
                                            rMap.BackColor = SMOvrBack2RGB
                                        End If
                                    End If
                                End If
                            End If
                            If Not IsNothing(ts.Font) Then
                                rMap.Font = ts.Font
                            ElseIf ts.IsOverride Then
                                rMap.Font = rFontS
                            End If

                        End If
                    Next

                    If isSel Then
                        If Not rSel.IsEmpty Then
                            rMap.BackColor = Helper.GetRGB(Helper.Blend(rSel, rBack, rSel.A / 255))
                        End If
                    End If

                    If (i >= charsLessPerRow And (Ix + Iy) + box.trans.LengthPerData <= QR.Length) Then
                        rMap.TextColor = SMGrayTextRGB
                        rMap.Font = SM.LessFont
                    ElseIf ((Ix + Iy) < QR.Length And Not (Ix + Iy) + box.trans.LengthPerData <= QR.Length) Then
                        rMap.TextColor = SMGrayTextRGB
                        rMap.Font = SM.LessFont
                    ElseIf (Ix + Iy) + box.trans.LengthPerData <= QR.Length Then

                    Else
                        rMap.TextColor = SMTextRGB
                        rMap.BackColor = SMBackRGB
                    End If


                    iZ += 1
                    If iZ >= perData Then
                        iZ = 0
                        Iy += box.trans.LengthPerData
                    End If
                Next

                Dim rectX As New Rectangle
                rX = 0
                Dim w As Integer = 0
                Dim wOrg As Integer = 0
                Dim rectXOrg As Integer = 0
                Dim charsPerRow2 As Integer = charsPerRow
                Dim LastRowChars As Integer = charsPerRow
                If y = ShowedRowCount - 1 Then
                    LastRowChars = LastRowLength Mod box.trans.LengthPerData
                    If LastRowChars = 0 Then
                        LastRowChars = LastRowLength / box.trans.LengthPerData * (box.trans.CharsPerData + box.trans.Sparator)
                    End If
                    charsPerRow2 = LastRowChars
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
                    rectX.X = rect.X + rX
                    rectX.Y = rect.Y
                    rectX.Width = w
                    rectX.Height = SM.row_height

                    If rectX.Contains(m) Then
                        Dim bakIndex As Integer = FocussedBoxIndex
                        Dim bakSL As SelectionManager = Me.SL.TransformSelection(SM.Box(FocussedBoxIndex).trans, box.trans).Clone
                        line = vscroll_val + y
                        Dim lineIndex As Long = line * charsPerRow
                        Dim colIndex As Integer = i
                        Dim absIndex As Long = lineIndex + colIndex

                        If LCtrlEnabled Then
                            If multisel Then
                                If Not FocussedBoxIndex = boxIndex Then
                                    Me.SL = bakSL.Clone
                                End If
                                Me.SL.AddSelection()
                            End If
                        Else
                            If selPower And multisel Then
                                If dbSnap Then
                                    Me.SL.Items.Clear()
                                Else
                                    If Not FocussedBoxIndex = boxIndex Then
                                        Me.SL = bakSL.Clone
                                    End If
                                    Me.SL.AddSelection()
                                End If
                            Else
                                Me.SL.Items.Clear()
                            End If
                        End If

                        Dim sItem As SelectionItem = Me.SL.Curent

                        If m.X > rectXOrg + wOrg Then
                            absIndex += 1
                            sItem.LF = True
                        Else
                            sItem.LF = False
                        End If
                        sItem.anc = absIndex
                        If fullRowSelection Then
                            sItem.car = absIndex + charsPerRow
                            sItem.LF = True
                        Else
                            sItem.car = absIndex
                            If dbSnap Or fullColSelection Then
                                SL.SnapSelection(sItem, SnapEffect.Anchor)
                                Dim trans As ITransformer = SM.Box(FocussedBoxIndex).trans
                                perData = trans.CharsPerData + trans.Sparator
                                Dim cr As Integer = (sItem.anc Mod charsPerRow) + perData
                                If cr > charsPerRow2 Then
                                    cr = charsPerRow2 - (sItem.anc Mod charsPerRow)
                                Else
                                    cr = perData
                                End If
                                sItem.car = sItem.anc + cr

                                InvalidateSEL(SL, sItem)
                            End If
                        End If
                        FocussedBoxIndex = boxIndex

                        Dim evt As New HexBoxSelectionEventArgs(bakIndex, bakSL, FocussedBoxIndex, Me.SL)
                        RaiseEvent SelectionChanged(Me, evt)
                        If evt.Cancel Then
                            FocussedBoxIndex = evt.OldBoxIndex
                            Me.SL = evt.OldSelection
                        End If

                        InvalidateSEL(bakSL)
                        InvalidateSEL(Me.SL)
                        Exit Sub
                    End If
                    rX += w
                Next

                rect.Y += SM.row_height
                For i As Integer = 0 To rMaps.Length - 1
                    rMaps(i).Reset()
                Next

                rGroups.Clear()
            Next
            rect.X += box.w
        Next
    End Sub
    Friend Sub MouseMoveEvent()
        Dim A As Long = vscroll_val * SM.col_count
        Dim rect As New Rectangle(SM.all_xpad, SM.all_ypad, MyRect.Width, MyRect.Height)
        If Not SM.offset_mode = OffsetMode.Hidden Then
            rect.X += SM.offsetBox_width
        End If

        If rect.Height <= 0 Then Exit Sub
        rect.X -= hscroll_val

        Dim fd As FontData = Nothing
        If Not SM.header_mode = HeaderMode.Hidden Then
            rect.Y += SM.header_height + SM.content_ypad
        End If

        rect.Height = SM.row_height

        Dim bY As Integer = rect.Y
        Dim ShowedRowCount As Integer = Me.ShowedRowCount
        Dim remainData As Long = bb.GetLength - A
        Dim remainRow As Long = Math.Ceiling(remainData / SM.col_count)
        Dim LastRowLength As Integer = SM.col_count
        If ShowedRowCount >= remainRow Then
            ShowedRowCount = remainRow
            LastRowLength = remainData Mod SM.col_count
            If LastRowLength = 0 Then LastRowLength = SM.col_count
        End If
        Dim SMBackRGB As Integer ' = Helper.GetRGB(SM.bkClr)
        Dim SMTextRGB As Integer ' = Helper.GetRGB(Helper.Blend(SM.txClr, SM.bkClr, SM.txClr.A / 255))
        Dim SMSelTextRGB As Integer ' = Helper.GetRGB(Helper.Blend(SM.seltxClr, SM.bkClr, SM.seltxClr.A / 255))
        Dim SMSelBackRGB As Integer ' = Helper.GetRGB(Helper.Blend(SM.selbkClr, SM.bkClr, SM.selbkClr.A / 255))
        Dim SMOvrTextRGB As Integer ' = Helper.GetRGB(Helper.Blend(SM.ovrtxClr, SM.bkClr, SM.ovrtxClr.A / 255))
        Dim SMOvrBackRGB As Integer '= Helper.GetRGB(Helper.Blend(SM.ovrbkClr, SM.bkClr, SM.ovrbkClr.A / 255))
        Dim SMOvrText2RGB As Integer ' = Helper.GetRGB(Helper.Blend(SM.ovrtx2Clr, SM.bkClr, SM.ovrtx2Clr.A / 255))
        Dim SMOvrBack2RGB As Integer ' = Helper.GetRGB(Helper.Blend(SM.ovrbk2Clr, SM.bkClr, SM.ovrbk2Clr.A / 255))
        Dim SMGrayTextRGB As Integer ' = Helper.GetRGB(Helper.Blend(SM.GrayTextColor, SM.bkClr, SM.GrayTextColor.A / 255))
        Dim SMBackBlendRGB As Integer '= Helper.GetRGB(Helper.Blend(SM.highLightLineColor, SM.bkClr, SM.highLightLineColor.A / 255))
        Dim rBackS As Color = Nothing
        Dim rSelS As Color = Nothing
        Dim rFontS As Font = Nothing
        Dim boxHeight As Integer = SM.row_height * ShowedRowCount
        Dim sourceBox As BoxItem = SM.Box(FocussedBoxIndex)
        Dim sItem2 As SelectionItem = Me.SL.Curent
        Dim caretz As Long = sItem2.car
        If sItem2.LF Then caretz -= 1
        Dim line As Long = Math.Floor(caretz / GetCharsPerRow(sourceBox))
        Dim index As Long = line * SM.col_count
        Dim isHighLight As Boolean = False
        If Not SM.header_mode = HeaderMode.Hidden Then
            boxHeight += SM.header_height + SM.content_ypad
        End If
        Dim m As Point = Me.m
        If m.X < rect.X Then
            m.X = rect.X
        End If
        Dim boxWidth As Integer = 0
        For Each i In SM.Box
            boxWidth += i.w + i.xpad
        Next
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
            rect.X += box.xpad
            '  If box Is sourceBox Then

            Dim SL As SelectionManager = Nothing
            If sourceBox Is box Then
                SL = Me.SL
            Else
                SL = Me.SL.TransformSelection(sourceBox.trans, box.trans)
            End If
            If IsNothing(box.Style) Then
                SMBackRGB = Helper.GetRGB(SM.bkClr)
                SMTextRGB = Helper.GetRGB(Helper.Blend(SM.txClr, SM.bkClr, SM.txClr.A / 255))
                SMSelTextRGB = Helper.GetRGB(Helper.Blend(SM.seltxClr, SM.bkClr, SM.seltxClr.A / 255))
                SMSelBackRGB = Helper.GetRGB(Helper.Blend(SM.selbkClr, SM.bkClr, SM.selbkClr.A / 255))
                SMOvrTextRGB = Helper.GetRGB(Helper.Blend(SM.ovrtxClr, SM.bkClr, SM.ovrtxClr.A / 255))
                SMOvrBackRGB = Helper.GetRGB(Helper.Blend(SM.ovrbkClr, SM.bkClr, SM.ovrbkClr.A / 255))
                SMOvrText2RGB = Helper.GetRGB(Helper.Blend(SM.ovrtx2Clr, SM.bkClr, SM.ovrtx2Clr.A / 255))
                SMOvrBack2RGB = Helper.GetRGB(Helper.Blend(SM.ovrbk2Clr, SM.bkClr, SM.ovrbk2Clr.A / 255))
                SMGrayTextRGB = Helper.GetRGB(Helper.Blend(SM.graytxClr, SM.bkClr, SM.graytxClr.A / 255))
                SMBackBlendRGB = Helper.GetRGB(Helper.Blend(SM.highLightLineColor, SM.bkClr, SM.highLightLineColor.A / 255))
                rBackS = SM.bkClr
                rSelS = SM.selbkClr
                rFontS = SM.Font
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
                rFontS = sts.Font
                SMBackRGB = Helper.GetRGB(sts.BackColor)
                SMTextRGB = Helper.GetRGB(Helper.Blend(sts.TextColor, sts.BackColor, sts.TextColor.A / 255))
                SMSelTextRGB = Helper.GetRGB(Helper.Blend(sts.HightlightTextColor, sts.BackColor, sts.HightlightTextColor.A / 255))
                SMSelBackRGB = Helper.GetRGB(Helper.Blend(sts.HightlightBackColor, sts.BackColor, sts.HightlightBackColor.A / 255))
                SMOvrTextRGB = Helper.GetRGB(Helper.Blend(sts.HotTextColor, sts.BackColor, sts.HotTextColor.A / 255))
                SMOvrBackRGB = Helper.GetRGB(Helper.Blend(sts.HotBackColor, sts.BackColor, sts.HotBackColor.A / 255))
                SMOvrText2RGB = Helper.GetRGB(Helper.Blend(sts.HotTextColor2, sts.BackColor, sts.HotTextColor2.A / 255))
                SMOvrBack2RGB = Helper.GetRGB(Helper.Blend(sts.HotBackColor2, sts.BackColor, sts.HotBackColor2.A / 255))
                SMGrayTextRGB = Helper.GetRGB(Helper.Blend(sts.GrayTextColor, sts.BackColor, sts.GrayTextColor.A / 255))
                SMBackBlendRGB = Helper.GetRGB(Helper.Blend(SM.highLightLineColor, sts.BackColor, SM.highLightLineColor.A / 255))
                rBackS = sts.BackColor
                rSelS = sts.HightlightBackColor
            End If
            Dim N As Long = 0
            Dim Ix As Integer = 0
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

            Dim isSel As Boolean = False
            Dim isContains As Boolean = False
            Dim rBack As Color = Nothing
            Dim rSel As Color = Nothing
            For y As Integer = 0 To ShowedRowCount - 1
                Ix = (A - QR.Position) + y * SM.col_count
                N = (vscroll_val + y) * SM.col_count
                isHighLight = N <= index And index < (N + SM.col_count)
                isHighLight = isHighLight And SM.highlightLine

                Tx = ""
                Iy = 0
                For i As Integer = 0 To SM.col_count - 1 Step box.trans.LengthPerData
                    If i >= colFloor Then
                        For i2 As Integer = colFloor To SM.col_count - 1
                            If (Ix + i) < QR.Length Then
                                Tx &= Helper.Hex2(QR.Buffer(Ix + i2))
                            Else
                                Tx &= "".PadRight(2)
                            End If
                        Next
                        Tx &= "".PadRight(box.trans.Sparator)
                    Else
                        If (Ix + i) < QR.Length And (Ix + i) + box.trans.LengthPerData <= QR.Length Then
                            Tx &= box.trans.GetString(QR.Buffer, Ix + i).PadRight(perData)
                        ElseIf (Ix + i) < QR.Length Then
                            For i2 As Integer = 0 To (Ix + i) + box.trans.LengthPerData - QR.Length - 1
                                If (Ix + i) < QR.Length Then
                                    Tx &= Helper.Hex2(QR.Buffer(Ix + i + i2)).PadRight(box.trans.CharsPerData)
                                Else
                                    Tx &= "".PadRight(2)
                                End If
                            Next
                            Tx &= "".PadRight(box.trans.Sparator)
                        Else
                            Tx &= "".PadRight(perData)
                        End If
                    End If
                    Iy += perData
                Next

                Iy = 0
                Dim iZ As Integer = 0
                For i As Integer = 0 To charsPerRow - 1
                    Idx = (A + y * SM.col_count) / SM.col_count * charsPerRow + i

                    rMap = rMaps(i)
                    rMap.Font = rFontS

                    isSel = SL.IsSelected(Idx)
                    If isHighLight Then
                        If isSel Then
                            rMap.TextColor = SMSelTextRGB
                            rMap.BackColor = SMSelBackRGB
                            rSel = rSelS
                        Else
                            rMap.TextColor = SMTextRGB
                            rMap.BackColor = SMBackBlendRGB
                        End If
                    Else
                        If isSel Then
                            rMap.TextColor = SMSelTextRGB
                            rMap.BackColor = SMSelBackRGB
                            rSel = rSelS
                        Else
                            rMap.TextColor = SMTextRGB
                            rMap.BackColor = SMBackRGB
                        End If
                    End If
                    rBack = rBackS

                    If SL.SelectionLength = 0 And SM.wMode = WriteMode.Over And SM.ovrMode.HasFlag(OverModes.Color) Then
                        If FocussedBoxIndex = boxIndex Then
                            If SL.IsOver(Idx, sourceBox.trans, sourceBox.trans) Then
                                If FocussedBoxIndex = boxIndex Then
                                    rMap.TextColor = SMOvrTextRGB
                                    rMap.BackColor = SMOvrBackRGB
                                Else
                                    rMap.TextColor = SMOvrText2RGB
                                    rMap.BackColor = SMOvrBack2RGB
                                End If
                            End If
                        Else
                            If Me.SL.IsOver(Idx, sourceBox.trans, box.trans) Then
                                If FocussedBoxIndex = boxIndex Then
                                    rMap.TextColor = SMOvrTextRGB
                                    rMap.BackColor = SMOvrBackRGB
                                Else
                                    rMap.TextColor = SMOvrText2RGB
                                    rMap.BackColor = SMOvrBack2RGB
                                End If
                            End If
                        End If
                    End If

                    For Each ts In SM.TSS
                        isContains = False
                        If ts.StyleTarget = StyleTarget.All Or ts.StyleTarget = StyleTarget.AllContents Or ts.StyleTarget = StyleTarget.SelectedContents Then
                            Dim continuex As Boolean = False
                            If ts.StyleTarget = StyleTarget.SelectedContents Then
                                continuex = ts.BoxIndex.Contains(boxIndex)
                            Else
                                continuex = True
                            End If
                            If continuex Then
                                If ts.Unit = PointUnit.Byte Then
                                    If ts.Contains(Ix + Iy) Then
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
                                    rMap.TextColor = SMSelTextRGB
                                End If
                                If Not ts.HightlightBackColor.IsEmpty Then
                                    rMap.BackColor = Helper.GetRGB(ts.HightlightBackColor)
                                    rSel = ts.HightlightBackColor
                                ElseIf ts.IsOverride Then
                                    rMap.BackColor = SMSelBackRGB
                                    rSel = rSelS
                                End If
                            Else
                                If Not ts.TextColor.IsEmpty Then
                                    rMap.TextColor = Helper.GetRGB(ts.TextColor)
                                ElseIf ts.IsOverride Then
                                    rMap.TextColor = SMTextRGB
                                End If
                                If Not ts.BackColor.IsEmpty Then
                                    rMap.BackColor = Helper.GetRGB(Helper.Blend(ts.BackColor, rBackS, ts.BackColor.A / 255))
                                ElseIf ts.IsOverride Then
                                    rMap.BackColor = SMBackRGB
                                End If
                            End If

                            If Not ts.BackColor.IsEmpty Then
                                rBack = Helper.Blend(ts.BackColor, rBackS, ts.BackColor.A / 255)
                            ElseIf ts.IsOverride Then
                                rBack = rBackS
                            End If

                            If SL.SelectionLength = 0 And SM.wMode = WriteMode.Over Then
                                If SL.IsCaret(Idx) Then
                                    If FocussedBoxIndex = boxIndex Then
                                        If Not ts.HotTextColor.IsEmpty Then
                                            rMap.TextColor = Helper.GetRGB(ts.HotTextColor)
                                        ElseIf ts.IsOverride Then
                                            rMap.TextColor = SMOvrTextRGB
                                        End If
                                        If Not ts.HotBackColor.IsEmpty Then
                                            rMap.BackColor = Helper.GetRGB(ts.HotBackColor)
                                        ElseIf ts.IsOverride Then
                                            rMap.BackColor = SMOvrBackRGB
                                        End If
                                    Else
                                        If Not ts.HotTextColor2.IsEmpty Then
                                            rMap.TextColor = Helper.GetRGB(ts.HotTextColor2)
                                        ElseIf ts.IsOverride Then
                                            rMap.TextColor = SMOvrText2RGB
                                        End If
                                        If Not ts.HotBackColor2.IsEmpty Then
                                            rMap.BackColor = Helper.GetRGB(ts.HotBackColor2)
                                        ElseIf ts.IsOverride Then
                                            rMap.BackColor = SMOvrBack2RGB
                                        End If
                                    End If
                                End If
                            End If
                            If Not IsNothing(ts.Font) Then
                                rMap.Font = ts.Font
                            ElseIf ts.IsOverride Then
                                rMap.Font = rFontS
                            End If

                        End If
                    Next

                    If isSel Then
                        If Not rSel.IsEmpty Then
                            rMap.BackColor = Helper.GetRGB(Helper.Blend(rSel, rBack, rSel.A / 255))
                        End If
                    End If

                    If (i >= charsLessPerRow And (Ix + Iy) + box.trans.LengthPerData <= QR.Length) Then
                        rMap.TextColor = SMGrayTextRGB
                        rMap.Font = SM.LessFont
                    ElseIf ((Ix + Iy) < QR.Length And Not (Ix + Iy) + box.trans.LengthPerData <= QR.Length) Then
                        rMap.TextColor = SMGrayTextRGB
                        rMap.Font = SM.LessFont
                    ElseIf (Ix + Iy) + box.trans.LengthPerData <= QR.Length Then

                    Else
                        rMap.TextColor = SMTextRGB
                        rMap.BackColor = SMBackRGB
                    End If


                    iZ += 1
                    If iZ >= perData Then
                        iZ = 0
                        Iy += box.trans.LengthPerData
                    End If
                Next

                Dim rectX As New Rectangle
                rX = 0
                Dim w As Integer = 0
                Dim wOrg As Integer = 0
                Dim charsPerRow2 As Integer = charsPerRow
                Dim LastRowChars As Integer = -1
                If y = ShowedRowCount - 1 Then
                    LastRowChars = LastRowLength Mod box.trans.LengthPerData
                    If LastRowChars = 0 Then
                        LastRowChars = LastRowLength / box.trans.LengthPerData * (box.trans.CharsPerData + box.trans.Sparator)
                    End If
                    charsPerRow2 = LastRowChars
                End If

                For i As Integer = 0 To charsPerRow2 - 1
                    rMap = rMaps(i)
                    fd = GetFontData(rMap.Font)
                    subTx = Mid(Tx, i + 1, 1)
                    w = Helper.GetTextWidth(fd.ABC, subTx)
                    wOrg = w
                    If i = charsPerRow2 - 1 Then
                        If boxIndex = SM.Box.Count - 1 Then
                            w = MyRect.Width
                        Else
                            w = box.w - rX
                        End If
                    ElseIf i = 0 Then
                        rX -= box.xpad
                        w += box.xpad
                    End If
                    rectX.X = rect.X + rX
                    rectX.Y = rect.Y
                    rectX.Width = w
                    If LastRowChars = -1 Then
                        rectX.Height = SM.row_height
                    Else
                        rectX.Height = MyRect.Height
                    End If

                    'If boxIndex = 0 Then Me.Parent.Text = m.ToString & " - " & rectX.ToString

                    If rectX.Contains(m) Then
                        Dim bakIndex As Integer = FocussedBoxIndex
                        Dim bakSL As SelectionManager = Me.SL.Clone
                        Dim bakItem As SelectionItem = Me.SL.Curent.Clone

                        line = vscroll_val + y
                        Dim lineIndex As Long = line * charsPerRow
                        Dim colIndex As Integer = i
                        Dim absIndex As Long = lineIndex + colIndex
                        Dim sItem As SelectionItem = Me.SL.Curent

                        If m.X > rectX.X + wOrg Then
                            sItem.LF = False
                            If i = charsPerRow2 - 1 Then
                                absIndex += 1
                                If charsPerRow = charsPerRow2 Then
                                    sItem.LF = True
                                    dontSnap = True
                                End If
                            ElseIf i = 0 Then
                                dontSnap = True
                            End If
                        Else
                            sItem.LF = False
                        End If
                        If Not FocussedBoxIndex = boxIndex Then
                            sItem2 = Me.SL.TransformItem(sItem, sourceBox.trans, box.trans)
                            FocussedBoxIndex = boxIndex
                            sItem.anc = sItem2.anc
                        End If
                        If fullRowSelection Then
                            If sItem.car > sItem.anc Then
                                sItem.car = lineIndex + charsPerRow2
                                If charsPerRow = charsPerRow2 Then sItem.LF = True
                            Else
                                sItem.car = lineIndex '+ charsPerRow2
                                'sItem.car = absIndex
                            End If
                        Else
                            sItem.car = absIndex
                        End If

                        Dim item As SelectionItem = Nothing
                        If dbSnap Then
                            item = New SelectionItem
                            item.anc = sItem.car - 2
                            item.LF = sItem.LF
                            Me.SL.SnapSelection(sItem, SnapEffect.Caret)
                            item.car = sItem.car
                            InvalidateSEL(Me.SL, item)
                        End If

                        item = New SelectionItem
                        item.anc = bakItem.car
                        item.car = sItem.car
                        item.LF = sItem.LF
                        InvalidateSEL(Me.SL, item)

                        If fullRowSelection And Not charsPerRow = charsPerRow2 Then
                            item = New SelectionItem
                            item.anc = bakItem.car
                            item.car = absIndex - 1
                            item.LF = sItem.LF
                            InvalidateSEL(Me.SL, item)
                        End If

                        item = New SelectionItem
                        item.anc = bakItem.anc
                        item.car = sItem.anc
                        item.LF = sItem.LF

                        InvalidateSEL(Me.SL, item)

                        Dim evt As New HexBoxSelectionEventArgs(bakIndex, bakSL, FocussedBoxIndex, Me.SL)
                        RaiseEvent SelectionChanged(Me, evt)
                        If evt.Cancel Then
                            FocussedBoxIndex = evt.OldBoxIndex
                            Me.SL = evt.OldSelection
                            InvalidateSEL(evt.OldSelection)
                            InvalidateSEL(evt.NewSelection)
                        End If

                        Exit Sub
                    End If
                    rX += w
                Next

                rect.Y += SM.row_height
                For i As Integer = 0 To rMaps.Length - 1
                    rMaps(i).Reset()
                Next

                rGroups.Clear()
            Next
            '  End If
            rect.X += box.w
        Next
    End Sub
    Friend Sub ScrollMoveEvent()
        Dim vscroll As Integer = 0
        Dim hscroll As Integer = 0

        Dim maxLength As Long = Math.Ceiling(bb.GetLength / SM.col_count)
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
                vscroll = vscroll / maxScroll * vscroll_max
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
        Dim rect As New Rectangle(SM.all_xpad, SM.all_ypad, MyRect.Width, MyRect.Height)

        If rect.Height <= 0 Then Exit Sub
        rect.X -= hscroll_val

        Dim cur As Integer = 1 '0=default '1=ibeam '2=reverse
        If IsInHighlight() Then
            cur = 0
        End If
        If Not SM.offset_mode = OffsetMode.Hidden Then
            If m.X < rect.X + SM.offsetBox_width Then
                cur = 2
            End If
        End If
        If Not SM.header_mode = HeaderMode.Hidden Then
            If m.Y <= SM.header_height Then
                cur = 0
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
        Dim rect As New Rectangle(SM.all_xpad, SM.all_ypad, MyRect.Width, MyRect.Height)
        If Not SM.offset_mode = OffsetMode.Hidden Then
            rect.X += SM.offsetBox_width
        End If

        If rect.Height <= 0 Then Return False
        rect.X -= hscroll_val

        Dim fd As FontData = Nothing
        If Not SM.header_mode = HeaderMode.Hidden Then
            rect.Y += SM.header_height + SM.content_ypad
        End If

        rect.Height = SM.row_height

        Dim bY As Integer = rect.Y

        Dim ShowedRowCount As Integer = Me.ShowedRowCount
        Dim remainData As Long = bb.GetLength - A
        Dim remainRow As Long = Math.Ceiling(remainData / SM.col_count)
        Dim LastRowLength As Integer = SM.col_count
        If ShowedRowCount >= remainRow Then
            ShowedRowCount = remainRow
            LastRowLength = remainData Mod SM.col_count
            If LastRowLength = 0 Then LastRowLength = SM.col_count
        End If

        Dim boxHeight As Integer = SM.row_height * ShowedRowCount

        If Not SM.header_mode = HeaderMode.Hidden Then
            boxHeight += SM.header_height + SM.content_ypad
        End If
        Dim SMBackRGB As Integer ' = Helper.GetRGB(SM.bkClr)
        Dim SMTextRGB As Integer ' = Helper.GetRGB(Helper.Blend(SM.txClr, SM.bkClr, SM.txClr.A / 255))
        Dim SMSelTextRGB As Integer ' = Helper.GetRGB(Helper.Blend(SM.seltxClr, SM.bkClr, SM.seltxClr.A / 255))
        Dim SMSelBackRGB As Integer ' = Helper.GetRGB(Helper.Blend(SM.selbkClr, SM.bkClr, SM.selbkClr.A / 255))
        Dim SMOvrTextRGB As Integer ' = Helper.GetRGB(Helper.Blend(SM.ovrtxClr, SM.bkClr, SM.ovrtxClr.A / 255))
        Dim SMOvrBackRGB As Integer '= Helper.GetRGB(Helper.Blend(SM.ovrbkClr, SM.bkClr, SM.ovrbkClr.A / 255))
        Dim SMOvrText2RGB As Integer ' = Helper.GetRGB(Helper.Blend(SM.ovrtx2Clr, SM.bkClr, SM.ovrtx2Clr.A / 255))
        Dim SMOvrBack2RGB As Integer ' = Helper.GetRGB(Helper.Blend(SM.ovrbk2Clr, SM.bkClr, SM.ovrbk2Clr.A / 255))
        Dim SMGrayTextRGB As Integer ' = Helper.GetRGB(Helper.Blend(SM.GrayTextColor, SM.bkClr, SM.GrayTextColor.A / 255))
        Dim SMBackBlendRGB As Integer '= Helper.GetRGB(Helper.Blend(SM.highLightLineColor, SM.bkClr, SM.highLightLineColor.A / 255))
        Dim rBackS As Color = Nothing
        Dim rSelS As Color = Nothing
        Dim rFontS As Font = Nothing

        Dim sourceBox As BoxItem = SM.Box(FocussedBoxIndex)
        Dim sItem2 As SelectionItem = Me.SL.Curent
        Dim caretz As Long = sItem2.car
        If sItem2.LF Then caretz -= 1
        Dim line As Long = Math.Floor(caretz / GetCharsPerRow(sourceBox))
        Dim index As Long = line * SM.col_count
        Dim isHighLight As Boolean = False

        For boxIndex As Integer = 0 To SM.Box.Count - 1
            Dim box As BoxItem = SM.Box(boxIndex)
            rect.X += box.xpad

            Dim SL As SelectionManager = Nothing
            If sourceBox Is box Then
                SL = Me.SL
            Else
                SL = Me.SL.TransformSelection(sourceBox.trans, box.trans)
            End If
            If IsNothing(box.Style) Then
                SMBackRGB = Helper.GetRGB(SM.bkClr)
                SMTextRGB = Helper.GetRGB(Helper.Blend(SM.txClr, SM.bkClr, SM.txClr.A / 255))
                SMSelTextRGB = Helper.GetRGB(Helper.Blend(SM.seltxClr, SM.bkClr, SM.seltxClr.A / 255))
                SMSelBackRGB = Helper.GetRGB(Helper.Blend(SM.selbkClr, SM.bkClr, SM.selbkClr.A / 255))
                SMOvrTextRGB = Helper.GetRGB(Helper.Blend(SM.ovrtxClr, SM.bkClr, SM.ovrtxClr.A / 255))
                SMOvrBackRGB = Helper.GetRGB(Helper.Blend(SM.ovrbkClr, SM.bkClr, SM.ovrbkClr.A / 255))
                SMOvrText2RGB = Helper.GetRGB(Helper.Blend(SM.ovrtx2Clr, SM.bkClr, SM.ovrtx2Clr.A / 255))
                SMOvrBack2RGB = Helper.GetRGB(Helper.Blend(SM.ovrbk2Clr, SM.bkClr, SM.ovrbk2Clr.A / 255))
                SMGrayTextRGB = Helper.GetRGB(Helper.Blend(SM.graytxClr, SM.bkClr, SM.graytxClr.A / 255))
                SMBackBlendRGB = Helper.GetRGB(Helper.Blend(SM.highLightLineColor, SM.bkClr, SM.highLightLineColor.A / 255))
                rBackS = SM.bkClr
                rSelS = SM.selbkClr
                rFontS = SM.Font
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
                rFontS = sts.Font
                SMBackRGB = Helper.GetRGB(sts.BackColor)
                SMTextRGB = Helper.GetRGB(Helper.Blend(sts.TextColor, sts.BackColor, sts.TextColor.A / 255))
                SMSelTextRGB = Helper.GetRGB(Helper.Blend(sts.HightlightTextColor, sts.BackColor, sts.HightlightTextColor.A / 255))
                SMSelBackRGB = Helper.GetRGB(Helper.Blend(sts.HightlightBackColor, sts.BackColor, sts.HightlightBackColor.A / 255))
                SMOvrTextRGB = Helper.GetRGB(Helper.Blend(sts.HotTextColor, sts.BackColor, sts.HotTextColor.A / 255))
                SMOvrBackRGB = Helper.GetRGB(Helper.Blend(sts.HotBackColor, sts.BackColor, sts.HotBackColor.A / 255))
                SMOvrText2RGB = Helper.GetRGB(Helper.Blend(sts.HotTextColor2, sts.BackColor, sts.HotTextColor2.A / 255))
                SMOvrBack2RGB = Helper.GetRGB(Helper.Blend(sts.HotBackColor2, sts.BackColor, sts.HotBackColor2.A / 255))
                SMGrayTextRGB = Helper.GetRGB(Helper.Blend(sts.GrayTextColor, sts.BackColor, sts.GrayTextColor.A / 255))
                SMBackBlendRGB = Helper.GetRGB(Helper.Blend(SM.highLightLineColor, sts.BackColor, SM.highLightLineColor.A / 255))
                rBackS = sts.BackColor
                rSelS = sts.HightlightBackColor
            End If
            Dim N As Long = 0
            Dim Ix As Integer = 0
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

            Dim isSel As Boolean = False
            Dim isContains As Boolean = False
            Dim rBack As Color = Nothing
            Dim rSel As Color = Nothing

            For y As Integer = 0 To ShowedRowCount - 1
                Ix = (A - QR.Position) + y * SM.col_count
                N = (vscroll_val + y) * SM.col_count
                isHighLight = N <= index And index < (N + SM.col_count)
                isHighLight = isHighLight And SM.highlightLine

                Tx = ""
                Iy = 0
                For i As Integer = 0 To SM.col_count - 1 Step box.trans.LengthPerData
                    If i >= colFloor Then
                        For i2 As Integer = colFloor To SM.col_count - 1
                            If (Ix + i) < QR.Length Then
                                Tx &= Helper.Hex2(QR.Buffer(Ix + i2))
                            Else
                                Tx &= "".PadRight(2)
                            End If
                        Next
                        Tx &= "".PadRight(box.trans.Sparator)
                    Else
                        If (Ix + i) < QR.Length And (Ix + i) + box.trans.LengthPerData <= QR.Length Then
                            Tx &= box.trans.GetString(QR.Buffer, Ix + i).PadRight(perData)
                        ElseIf (Ix + i) < QR.Length Then
                            For i2 As Integer = 0 To (Ix + i) + box.trans.LengthPerData - QR.Length - 1
                                If (Ix + i) < QR.Length Then
                                    Tx &= Helper.Hex2(QR.Buffer(Ix + i + i2)).PadRight(box.trans.CharsPerData)
                                Else
                                    Tx &= "".PadRight(2)
                                End If
                            Next
                            Tx &= "".PadRight(box.trans.Sparator)
                        Else
                            Tx &= "".PadRight(perData)
                        End If
                    End If
                    Iy += perData
                Next

                Iy = 0
                Dim iZ As Integer = 0
                For i As Integer = 0 To charsPerRow - 1
                    Idx = (A + y * SM.col_count) / SM.col_count * charsPerRow + i

                    rMap = rMaps(i)
                    rMap.Font = rFontS

                    isSel = SL.IsSelected(Idx)
                    If isHighLight Then
                        If isSel Then
                            rMap.TextColor = SMSelTextRGB
                            rMap.BackColor = SMSelBackRGB
                            rSel = rSelS
                        Else
                            rMap.TextColor = SMTextRGB
                            rMap.BackColor = SMBackBlendRGB
                        End If
                    Else
                        If isSel Then
                            rMap.TextColor = SMSelTextRGB
                            rMap.BackColor = SMSelBackRGB
                            rSel = rSelS
                        Else
                            rMap.TextColor = SMTextRGB
                            rMap.BackColor = SMBackRGB
                        End If
                    End If
                    rBack = rBackS

                    If SL.SelectionLength = 0 And SM.wMode = WriteMode.Over And SM.ovrMode.HasFlag(OverModes.Color) Then
                        If FocussedBoxIndex = boxIndex Then
                            If SL.IsOver(Idx, sourceBox.trans, sourceBox.trans) Then
                                If FocussedBoxIndex = boxIndex Then
                                    rMap.TextColor = SMOvrTextRGB
                                    rMap.BackColor = SMOvrBackRGB
                                Else
                                    rMap.TextColor = SMOvrText2RGB
                                    rMap.BackColor = SMOvrBack2RGB
                                End If
                            End If
                        Else
                            If Me.SL.IsOver(Idx, sourceBox.trans, box.trans) Then
                                If FocussedBoxIndex = boxIndex Then
                                    rMap.TextColor = SMOvrTextRGB
                                    rMap.BackColor = SMOvrBackRGB
                                Else
                                    rMap.TextColor = SMOvrText2RGB
                                    rMap.BackColor = SMOvrBack2RGB
                                End If
                            End If
                        End If
                    End If

                    For Each ts In SM.TSS
                        isContains = False
                        If ts.StyleTarget = StyleTarget.All Or ts.StyleTarget = StyleTarget.AllContents Or ts.StyleTarget = StyleTarget.SelectedContents Then
                            Dim continuex As Boolean = False
                            If ts.StyleTarget = StyleTarget.SelectedContents Then
                                continuex = ts.BoxIndex.Contains(boxIndex)
                            Else
                                continuex = True
                            End If
                            If continuex Then
                                If ts.Unit = PointUnit.Byte Then
                                    If ts.Contains(Ix + Iy) Then
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
                                    rMap.TextColor = SMSelTextRGB
                                End If
                                If Not ts.HightlightBackColor.IsEmpty Then
                                    rMap.BackColor = Helper.GetRGB(ts.HightlightBackColor)
                                    rSel = ts.HightlightBackColor
                                ElseIf ts.IsOverride Then
                                    rMap.BackColor = SMSelBackRGB
                                    rSel = rSelS
                                End If
                            Else
                                If Not ts.TextColor.IsEmpty Then
                                    rMap.TextColor = Helper.GetRGB(ts.TextColor)
                                ElseIf ts.IsOverride Then
                                    rMap.TextColor = SMTextRGB
                                End If
                                If Not ts.BackColor.IsEmpty Then
                                    rMap.BackColor = Helper.GetRGB(Helper.Blend(ts.BackColor, rBackS, ts.BackColor.A / 255))
                                ElseIf ts.IsOverride Then
                                    rMap.BackColor = SMBackRGB
                                End If
                            End If

                            If Not ts.BackColor.IsEmpty Then
                                rBack = Helper.Blend(ts.BackColor, rBackS, ts.BackColor.A / 255)
                            ElseIf ts.IsOverride Then
                                rBack = rBackS
                            End If

                            If SL.SelectionLength = 0 And SM.wMode = WriteMode.Over Then
                                If SL.IsCaret(Idx) Then
                                    If FocussedBoxIndex = boxIndex Then
                                        If Not ts.HotTextColor.IsEmpty Then
                                            rMap.TextColor = Helper.GetRGB(ts.HotTextColor)
                                        ElseIf ts.IsOverride Then
                                            rMap.TextColor = SMOvrTextRGB
                                        End If
                                        If Not ts.HotBackColor.IsEmpty Then
                                            rMap.BackColor = Helper.GetRGB(ts.HotBackColor)
                                        ElseIf ts.IsOverride Then
                                            rMap.BackColor = SMOvrBackRGB
                                        End If
                                    Else
                                        If Not ts.HotTextColor2.IsEmpty Then
                                            rMap.TextColor = Helper.GetRGB(ts.HotTextColor2)
                                        ElseIf ts.IsOverride Then
                                            rMap.TextColor = SMOvrText2RGB
                                        End If
                                        If Not ts.HotBackColor2.IsEmpty Then
                                            rMap.BackColor = Helper.GetRGB(ts.HotBackColor2)
                                        ElseIf ts.IsOverride Then
                                            rMap.BackColor = SMOvrBack2RGB
                                        End If
                                    End If
                                End If
                            End If
                            If Not IsNothing(ts.Font) Then
                                rMap.Font = ts.Font
                            ElseIf ts.IsOverride Then
                                rMap.Font = rFontS
                            End If

                        End If
                    Next

                    If isSel Then
                        If Not rSel.IsEmpty Then
                            rMap.BackColor = Helper.GetRGB(Helper.Blend(rSel, rBack, rSel.A / 255))
                        End If
                    End If

                    If (i >= charsLessPerRow And (Ix + Iy) + box.trans.LengthPerData <= QR.Length) Then
                        rMap.TextColor = SMGrayTextRGB
                        rMap.Font = SM.LessFont
                    ElseIf ((Ix + Iy) < QR.Length And Not (Ix + Iy) + box.trans.LengthPerData <= QR.Length) Then
                        rMap.TextColor = SMGrayTextRGB
                        rMap.Font = SM.LessFont
                    ElseIf (Ix + Iy) + box.trans.LengthPerData <= QR.Length Then

                    Else
                        rMap.TextColor = SMTextRGB
                        rMap.BackColor = SMBackRGB
                    End If


                    iZ += 1
                    If iZ >= perData Then
                        iZ = 0
                        Iy += box.trans.LengthPerData
                    End If
                Next

                Dim rectX As New Rectangle
                rX = 0
                Dim w As Integer = 0

                For i As Integer = 0 To charsPerRow - 1
                    rMap = rMaps(i)
                    fd = GetFontData(rMap.Font)
                    subTx = Mid(Tx, i + 1, 1)
                    w = Helper.GetTextWidth(fd.ABC, subTx)
                    rectX.X = rect.X + rX
                    rectX.Y = rect.Y
                    rectX.Width = w
                    rectX.Height = SM.row_height

                    If rectX.Contains(m) Then
                        Dim cpr As Integer = GetCharsPerRow(box)
                        line = (vscroll_val + y) * cpr
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
                For i As Integer = 0 To rMaps.Length - 1
                    rMaps(i).Reset()
                Next

                rGroups.Clear()
            Next
            rect.X += box.w
        Next
        Return False
    End Function
#End Region

#Region "Invalidator"
    Public Sub InvalidateSelection()
        InvalidateSEL(Me.SL)
    End Sub
    Public Sub InvalidateSelection(ByVal oldSelection As SelectionManager)
        InvalidateSEL(oldSelection)
        InvalidateSEL(Me.SL)
    End Sub
    Friend Sub InvalidateSEL(ByVal sl As SelectionManager, ByVal Items As SelectionItem())
        Dim A As Long = vscroll_val * SM.col_count
        Dim wContent As Integer = GetContentWidth()
        Dim rect As New Rectangle(SM.all_xpad, SM.all_ypad, wContent, MyRect.Height)

        If rect.Height <= 0 Then Exit Sub
        rect.X -= hscroll_val

        If Not SM.header_mode = HeaderMode.Hidden Then
            rect.Y += SM.header_height + SM.content_ypad
        End If

        rect.Height = SM.row_height

        Dim boxHeight As Integer = SM.row_height * (ShowedRowCount - 1)
        Dim box As BoxItem = SM.Box(FocussedBoxIndex)

        Dim perData As Integer = box.trans.CharsPerData + box.trans.Sparator
        Dim colFloor As Integer = Math.Floor(SM.col_count / box.trans.LengthPerData) * box.trans.LengthPerData
        Dim charsPerRow As Integer = (colFloor / box.trans.LengthPerData * perData)
        Dim charsLessPerRow As Integer = (colFloor / box.trans.LengthPerData * perData)
        Dim perLess As Integer = (SM.col_count - colFloor) * 2
        If (SM.col_count - colFloor) > 0 Then perLess += box.trans.Sparator
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
    Friend Sub InvalidateSEL(ByVal sl As SelectionManager, ByVal Item As SelectionItem)
        InvalidateSEL(sl, {Item})
    End Sub
    Friend Sub InvalidateSEL(ByVal sl As SelectionManager)
        InvalidateSEL(sl, sl.Items.ToArray)
    End Sub
    Friend Sub InvalidateCollumnHeader()
        If SM.header_mode = HeaderMode.Hidden Then Exit Sub
        Dim rect As New Rectangle(SM.all_xpad, SM.all_ypad + SM.header_ypad, GetContentWidth, SM.header_height)
        Me.Invalidate(rect)
    End Sub
    Friend Sub InvalidateLine(ByVal line As Long, ByVal count As Long)
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
            Dim rect As New Rectangle(SM.all_xpad, SM.all_ypad, wContent, SM.row_height)

            If rect.Height <= 0 Then Exit Sub
            rect.X -= hscroll_val

            If Not SM.header_mode = HeaderMode.Hidden Then
                rect.Y += SM.header_height + SM.content_ypad
            End If

            invalidLineStart = invalidLineStart - showLineStart
            invalidLineEnd = invalidLineEnd - showLineStart

            rect.Y += SM.row_height * invalidLineStart
            For y As Integer = invalidLineStart To invalidLineEnd
                Me.Invalidate(rect)
                rect.Y += SM.row_height
            Next
            Me.Update()
        End If
    End Sub
    Friend Sub InvalidateLine(ByVal line As Long)
        Dim showLineStart As Long = vscroll_val
        Dim showLineEnd As Long = vscroll_val + ShowedRowCount
        If Not (showLineStart <= line And line < showLineEnd) Then
            Exit Sub
        End If

        Dim wContent As Integer = GetContentWidth()
        Dim rect As New Rectangle(SM.all_xpad, SM.all_ypad, wContent, MyRect.Height)

        If rect.Height <= 0 Then Exit Sub
        rect.X -= hscroll_val

        If Not SM.header_mode = HeaderMode.Hidden Then
            rect.Y += SM.header_height + SM.content_ypad
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
            Dim rect As New Rectangle(SM.all_xpad, SM.all_ypad, wContent, SM.row_height)

            If rect.Height <= 0 Then Exit Sub
            rect.X -= hscroll_val

            If Not SM.header_mode = HeaderMode.Hidden Then
                rect.Y += SM.header_height + SM.content_ypad
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
            Me.Update()
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

#Region "Key Event Handler"
    Private Sub TransformBox_KeyDown(sender As Object, e As KeyEventArgs) Handles MyBase.KeyDown
        If e.KeyCode = Keys.ControlKey Then
            LCtrlEnabled = True
        End If
    End Sub

    Private Sub TransformBox_KeyUp(sender As Object, e As KeyEventArgs) Handles MyBase.KeyUp
        LCtrlEnabled = False
    End Sub

#End Region

#Region "Public"
    Friend FocussedBoxIndex As Integer = 0
    Friend extraW As Integer = 0

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
                Dim bakSL As SelectionManager = Me.SL.Clone

                Dim transformer As ITransformer = SM.Box(FocussedBoxIndex).trans
                Me.SL = Me.SL.TransformSelection(transformer, transformer)

                InvalidateSEL(bakSL)
                InvalidateSEL(Me.SL)
            End If
        End Set
    End Property
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
            line = line / maxScroll
        End If

        VScrollBar_SetValue(line)
        Dim v As Long = VScrollBar_GetValue()
        If vscroll_enhanced Then
            v = v / maxScroll * vscroll_max
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
        Dim maxLine As Long = Math.Floor(bb.GetLength / SM.col_count) - ShowedRowCount
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
            Return SM.draw_border
        End Get
        Set(value As Integer)
            If value < 0 Then value = 0
            If Not extraW = value Then
                extraW = value
                Me.Invalidate()
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
    Public Property OffsetAutoSize As Boolean
        Get
            Return SM.offsetAutoSize
        End Get
        Set(value As Boolean)
            If Not SM.offsetAutoSize = value Then
                SM.offsetAutoSize = value
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
#End Region

#Region "ByteBuilder Event Handler"
    Private Sub ByteBuilder_Load(sender As Object, e As EventArgs) Handles bb.Load
        Me.SL.Clear()
        RefreshInfo()
        Me.Invalidate()
        Me.Update()
    End Sub
    Private Sub ByteBuilder_NewUndoableState(sender As Object, e As UndoableByteBuilderEventArgs) Handles bb.NewUndoableState
    End Sub
    Private Sub ByteBuilder_LoadUndoableState(sender As Object, e As UndoableByteBuilderEventArgs) Handles bb.LoadUndoableState
    End Sub
    Private Sub ByteBuilder_ByteChanged(sender As Object, e As ByteChangedEventArgs) Handles bb.ByteChanged
        If e.Action = ActionTypes.Over Then
            InvalidatePOS(e.Position, e.Length)
        ElseIf e.Action = ActionTypes.Insert Or e.Action = ActionTypes.Remove Then
            RefreshInfoWithoutInvalidate()
            InvalidatePOS(e.Position, bb.GetLength - e.Position)
        End If

    End Sub
#End Region
#Region "ScrollBars"
    Friend vscroll_val As Long
    Friend hscroll_val As Long
    Friend maxScroll As Integer = 2000000000
    Friend vscroll_enhanced As Boolean
    Friend vscroll_max As Long
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
        Dim maxLength As Long = Math.Ceiling(bb.GetLength / SM.col_count)
        Dim vShow As Boolean = False
        If maxLength > maxScroll Then
            vscroll_enhanced = True
            vscroll_max = maxLength
            VScrollBar_SetInfo(0, maxScroll, ShowedRowCount)
            VScrollBar_Show(True)
            vShow = True
        ElseIf maxLength < ShowedRowCount Then
            vscroll_enhanced = False
            VScrollBar_SetInfo(0, maxLength, ShowedRowCount)
            VScrollBar_Show(False)
            ShowedStart = 0
        Else
            vscroll_enhanced = False
            VScrollBar_SetInfo(0, maxLength, ShowedRowCount)
            VScrollBar_Show(True)
            vShow = True
        End If
    End Sub
    Friend Sub RefreshHScrollBar()
        Dim inv As Boolean = False
        'hscroll
        Dim wcontent As Integer = SM.all_xpad + 25 + extraW
        If Not SM.offset_mode = OffsetMode.Hidden Then
            wcontent += SM.offsetBox_width
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
                v = v / maxScroll * vscroll_max
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
        Dim v As Long = e.Delta / -120
        VScrollBar_SetValue(VScrollBar_GetValue() + v)

        v = VScrollBar_GetValue()

        If vscroll_enhanced Then
            v = v / maxScroll * vscroll_max
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
            Me.Update()
        End If
    End Sub

    Friend Function GetContentWidth() As Integer
        Dim wcontent As Integer = SM.all_xpad
        If Not SM.offset_mode = OffsetMode.Hidden Then
            wcontent += SM.offsetBox_width
        End If
        For Each i In SM.Box
            wcontent += i.w + i.xpad
        Next
        Return wcontent
    End Function

#End Region
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

Public Class RenderChar
    Public TextColor As Integer
    Public BackColor As Integer
    Public Font As Font
    Public ReadOnly Property hFont As IntPtr
        Get
            Return HexBox.GetFontData(Font).hFont
        End Get
    End Property
    Public Sub Reset()
        TextColor = 0
        BackColor = 0
        Font = Nothing
    End Sub
End Class
Public Class RenderGroup
    Public Start As Integer
    Public Length As Integer
    Public Style As RenderChar
    Public Width As Single
    Public Height As Single

End Class