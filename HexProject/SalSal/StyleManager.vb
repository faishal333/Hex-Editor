Imports System.ComponentModel
Imports System.Drawing

Public Class StyleSet
    Inherits BaseStyle
    Friend tb As HexBox

    Friend txClr As Color
    Friend bkClr As Color
    Friend seltxClr As Color
    Friend selbkClr As Color
    Friend ovrtxClr As Color
    Friend ovrbkClr As Color
    Friend ovrtx2Clr As Color
    Friend ovrbk2Clr As Color
    Friend ovrtx3Clr As Color
    Friend ovrbk3Clr As Color
    Friend offsettxClr As Color
    Friend offsetbkClr As Color
    Friend headtxClr As Color
    Friend headbkClr As Color
    Friend graytxClr As Color
    Friend wrongtxClr As Color = Color.Red
    Friend ubtxClr As Color = Color.Gray
    Friend uetxClr As Color = Color.Blue
    Friend lineColor As Color = SystemColors.ActiveBorder
    Friend highLightLineColor As Color = Color.FromArgb(150, Color.LightGray)
    Friend slider_normal As Color = SystemColors.ScrollBar
    Friend slider_hot As Color = SystemColors.ControlLight
    Friend slider_down As Color = SystemColors.ControlDark
    Friend slider_border As Color = SystemColors.ScrollBar
    Friend slider_back As Color = Nothing

    Friend fnt As Font

    Friend header_mode As HeaderMode = HeaderMode.Default
    Friend offset_mode As OffsetMode = OffsetMode.Hex4
    Friend ovrMode As OverModes = OverModes.Color
    Friend wMode As WriteMode = WriteMode.Overwrite

    Friend draw_border As Boolean = False
    Friend AutoSnap As Boolean = True
    Friend usrpaint_head As Boolean = False
    Friend usrpaint_offset As Boolean = False
    Friend usrpaint_content As Boolean = False
    Friend hexSign As Boolean = True
    Friend offsetLiteMode As Boolean = True
    Friend offsetAutoSize As Boolean = True
    Friend highlightLine As Boolean = False
    Friend FullHighlightLine As Boolean = True
    Friend AutoCloseStream As Boolean = True

    Friend col_count As Integer = 8
    Friend row_height As Integer = 0
    Friend header_height As Integer = 0
    Friend offset_width As Integer = 0
    Friend all_padLeft As Integer = 0
    Friend all_padTop As Integer = 0
    Friend header_padTop As Integer = 5
    Friend content_padTop As Integer = 5
    Friend offset_padLeft As Integer = 10
    Friend offset_wx As Integer = 10
    Friend borderW As Integer = 1
    Friend indicatorSize As Integer = 1

    Friend hPatternBrush As IntPtr

    Friend TSS As New List(Of TextStyle)
    Friend Box As New List(Of BoxItem)

    Friend Shared Arial As New Font("Arial", 10, FontStyle.Regular)
    Friend Shared Consolas As New Font("Consolas", 10, FontStyle.Regular)
    Friend Shared CourierNew As New Font(FontFamily.GenericMonospace, 10, FontStyle.Regular)

    Friend Shared abcDatas As New List(Of FontData)

    Friend Sub New(ByVal tb As HexBox)
        Me.tb = tb
        txClr = SystemColors.WindowText
        bkClr = SystemColors.Window
        seltxClr = SystemColors.Window
        selbkClr = SystemColors.Highlight   'Helper.Blend(SystemColors.ControlDark, BackColor, 0.3)
        ovrtxClr = SystemColors.WindowText
        ovrbkClr = Color.FromArgb(150, Color.Yellow)  'SystemColors.ActiveCaption       ' Color.FromArgb(255, 255 - SelectedBackColor.R, 255 - SelectedBackColor.G, 255 - SelectedBackColor.B)
        ovrtx2Clr = SystemColors.WindowText
        ovrbk2Clr = Color.LightBlue     ' Color.LightGray
        ovrtx3Clr = Color.Red
        ovrbk3Clr = Color.FromArgb(150, Color.White)     ' Color.LightGray
        graytxClr = Color.White

        fnt = CourierNew

        offsettxClr = txClr
        offsetbkClr = SystemColors.Control
        headtxClr = txClr
        headbkClr = SystemColors.Control

        row_height = Helper.GetTextHeight(fnt)
        header_height = row_height + header_padTop * 2 - 2

        Dim cpr As Integer = offset_mode
        If offset_mode > 16 Then
            Select Case offset_mode
                Case OffsetMode.Bytes1
                    cpr = Math.Max(Byte.MinValue.ToString.Length, Byte.MaxValue.ToString.Length)
                Case OffsetMode.Bytes2
                    cpr = Math.Max(UShort.MinValue.ToString.Length, UShort.MaxValue.ToString.Length)
                Case OffsetMode.Bytes4
                    cpr = Math.Max(UInteger.MinValue.ToString.Length, UInteger.MaxValue.ToString.Length)
                Case OffsetMode.Bytes8
                    cpr = Math.Max(Long.MinValue.ToString.Length, Long.MaxValue.ToString.Length)
            End Select
        Else
            If hexSign Then
                cpr += 1
            End If
        End If

        Dim offsetS(cpr - 1) As Byte

        For i As Integer = 0 To cpr - 1
            offsetS(i) = 48
        Next

        Dim fd As FontData = GetFontData(fnt)
        offset_width = Helper.GetTextWidthA(fd.ABC, offsetS) + offset_padLeft * 2 + 1

        Dim b As BoxItem = Nothing

        b = New BoxItem(tb, TransformMode.HexView)
        Box.Add(b)
        b = New BoxItem(tb, TransformMode.CharView)
        Box.Add(b)

        UpdateBoxes()

        Dim bmp As New Bitmap(offset_wx, 2)
        Dim ax As Boolean = False
        For y As Integer = 0 To bmp.Height - 1
            For x As Integer = 0 To bmp.Width - 1
                If Not ax Then
                    bmp.SetPixel(x, y, offsetbkClr)
                Else
                    bmp.SetPixel(x, y, Color.Gray)
                End If
                ax = Not ax
            Next
            ax = Not ax
        Next

        Dim hbmp As IntPtr = bmp.GetHbitmap
        hPatternBrush = GDI32.CreatePatternBrush(hbmp)

    End Sub
    Friend Sub UpdateBoxes()
        For Each i In Me.Box
            If i.az Then UpdateBox(i)
        Next
    End Sub
    Friend Sub UpdateBox(ByVal box As BoxItem)
        If Not box.az Then Exit Sub
        Dim perData As Integer = box.trans.CharsPerData + box.trans.Sparator
        Dim colFloor As Integer = Math.Floor(col_count / box.trans.LengthPerData) * box.trans.LengthPerData
        Dim charsPerRow As Integer = (colFloor / box.trans.LengthPerData * perData)
        Dim perLess As Integer = (col_count - colFloor) * 2
        If (col_count - colFloor) > 0 Then perLess += box.trans.Sparator
        charsPerRow += perLess

        Dim textS(charsPerRow - 1) As Byte

        For i As Integer = 0 To charsPerRow - 1
            textS(i) = 48
        Next

        Dim fnt As Font = Me.fnt
        If Not IsNothing(box.ts) Then
            If Not IsNothing(box.ts.Font) Then
                fnt = box.ts.Font
            End If
        End If
        Dim fd As FontData = GetFontData(fnt)
        box.w = Helper.GetTextWidthA(fd.ABC, textS) + box.xpad * 2

    End Sub

    Friend Sub UpdateStyle()
        row_height = Helper.GetTextHeight(fnt)
        header_height = row_height + header_padTop * 2 - 2

        Dim cpr As Integer = offset_mode
        If offsetLiteMode Then
            Dim drawAddressStart As Long = tb.vscroll_val * col_count
            Dim Tx As String = ""
            If offset_mode > 16 Then
                Dim lmax As Integer = 0
                For i As Integer = 0 To tb.ShowedRowCount - 1
                    Dim adr As Long = drawAddressStart + i * col_count
                    Dim adrb As Byte() = BitConverter.GetBytes(adr)
                    Select Case offset_mode
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
                For i As Integer = 0 To tb.ShowedRowCount - 1
                    Dim adr As Long = drawAddressStart + i * col_count
                    Tx = Hex(adr)
                    If Tx.Length > lmax Then
                        lmax = Tx.Length
                    End If
                Next
                cpr = lmax
                If hexSign Then
                    cpr += 1
                End If
            End If

        Else
            If offset_mode > 16 Then
                Select Case offset_mode
                    Case OffsetMode.Bytes1
                        cpr = Math.Max(Byte.MinValue.ToString.Length, Byte.MaxValue.ToString.Length)
                    Case OffsetMode.Bytes2
                        cpr = Math.Max(UShort.MinValue.ToString.Length, UShort.MaxValue.ToString.Length)
                    Case OffsetMode.Bytes4
                        cpr = Math.Max(UInteger.MinValue.ToString.Length, UInteger.MaxValue.ToString.Length)
                    Case OffsetMode.Bytes8
                        cpr = Math.Max(Long.MinValue.ToString.Length, Long.MaxValue.ToString.Length)
                End Select
            Else
                If hexSign Then
                    cpr += 1
                End If
            End If
        End If

        If offsetAutoSize Then
            Dim offsetS(cpr - 1) As Byte

            For i As Integer = 0 To cpr - 1
                offsetS(i) = 48
            Next

            Dim fd As FontData = GetFontData(fnt)
            offset_width = Helper.GetTextWidthA(fd.ABC, offsetS) + offset_padLeft * 2 + 1
        End If

        UpdateBoxes()
    End Sub

    Friend Shared Function GetFontData(ByVal Font As Font) As FontData
        If IsNothing(Font) Then Return Nothing
        For Each i In abcDatas
            If i.Font Is Font Then
                Return i
            End If
        Next

        Dim newd As New FontData(Font)
        abcDatas.Add(newd)
        Return newd
    End Function

#Region "Public"
    Public Overrides Property TextColor As Color
        Get
            Return txClr
        End Get
        Set(value As Color)
            If Not txClr = value Then
                txClr = value
                tb.Invalidate()
            End If
        End Set
    End Property
    Public Overrides Property BackColor As Color
        Get
            Return bkClr
        End Get
        Set(value As Color)
            If Not bkClr = value Then
                bkClr = value
                tb.Invalidate()
            End If
        End Set
    End Property
    Public Property HighlightTextColor As Color
        Get
            Return seltxClr
        End Get
        Set(value As Color)
            If Not seltxClr = value Then
                seltxClr = value
                tb.Invalidate()
            End If
        End Set
    End Property
    Public Property HighlightBackColor As Color
        Get
            Return selbkClr
        End Get
        Set(value As Color)
            If Not selbkClr = value Then
                selbkClr = value
                tb.Invalidate()
            End If
        End Set
    End Property
    Public Overrides Property HotTextColor As Color
        Get
            Return ovrtxClr
        End Get
        Set(value As Color)
            If Not ovrtxClr = value Then
                ovrtxClr = value
                tb.Invalidate()
            End If
        End Set
    End Property
    Public Overrides Property HotBackColor As Color
        Get
            Return ovrbkClr
        End Get
        Set(value As Color)
            If Not ovrbkClr = value Then
                ovrbkClr = value
                tb.Invalidate()
            End If
        End Set
    End Property
    Public Overrides Property HotTextColor2 As Color
        Get
            Return ovrtx2Clr
        End Get
        Set(value As Color)
            If Not ovrtx2Clr = value Then
                ovrtx2Clr = value
                tb.Invalidate()
            End If
        End Set
    End Property
    Public Overrides Property HotBackColor2 As Color
        Get
            Return ovrbk2Clr
        End Get
        Set(value As Color)
            If Not ovrbk2Clr = value Then
                ovrbk2Clr = value
                tb.Invalidate()
            End If
        End Set
    End Property
    Public Overrides Property HotTextColor3 As Color
        Get
            Return ovrtx3Clr
        End Get
        Set(value As Color)
            If Not ovrtx3Clr = value Then
                ovrtx3Clr = value
                tb.Invalidate()
            End If
        End Set
    End Property
    Public Overrides Property HotBackColor3 As Color
        Get
            Return ovrbk3Clr
        End Get
        Set(value As Color)
            If Not ovrbk3Clr = value Then
                ovrbk3Clr = value
                tb.Invalidate()
            End If
        End Set
    End Property
    Public Overrides Property WrongTextColor As Color
        Get
            Return wrongtxClr
        End Get
        Set(value As Color)
            If Not wrongtxClr = value Then
                wrongtxClr = value
                tb.Invalidate()
            End If
        End Set
    End Property
    Public Overrides Property UnAccessableTextColor As Color
        Get
            Return ubtxClr
        End Get
        Set(value As Color)
            If Not ubtxClr = value Then
                ubtxClr = value
                tb.Invalidate()
            End If
        End Set
    End Property
    Public Overrides Property UnEditableTextColor As Color
        Get
            Return uetxClr
        End Get
        Set(value As Color)
            If Not uetxClr = value Then
                uetxClr = value
                tb.Invalidate()
            End If
        End Set
    End Property
    Public Property OffsetTextColor As Color
        Get
            Return offsettxClr
        End Get
        Set(value As Color)
            If Not offsettxClr = value Then
                offsettxClr = value
                tb.Invalidate()
            End If
        End Set
    End Property
    Public Property OffsetBackColor As Color
        Get
            Return offsetbkClr
        End Get
        Set(value As Color)
            If Not offsetbkClr = value Then
                offsetbkClr = value
                tb.Invalidate()
            End If
        End Set
    End Property
    Public Property CollumnHeaderTextColor As Color
        Get
            Return headtxClr
        End Get
        Set(value As Color)
            If Not headtxClr = value Then
                headtxClr = value
                tb.Invalidate()
            End If
        End Set
    End Property
    Public Property CollumnHeaderBackColor As Color
        Get
            Return headbkClr
        End Get
        Set(value As Color)
            If Not headbkClr = value Then
                headbkClr = value
                tb.Invalidate()
            End If
        End Set
    End Property
    Public Property HighlighRowColor As Color
        Get
            Return highLightLineColor
        End Get
        Set(value As Color)
            If Not highLightLineColor = value Then
                highLightLineColor = value
                tb.Invalidate()
            End If
        End Set
    End Property
    Public Property BorderColor As Color
        Get
            Return lineColor
        End Get
        Set(value As Color)
            If Not lineColor = value Then
                lineColor = value
                tb.Invalidate()
            End If
        End Set
    End Property
    Public Property BorderWidth As Integer
        Get
            Return borderW
        End Get
        Set(value As Integer)
            If Not borderW = value Then
                If value <= 0 Then
                    value = 0
                    draw_border = False
                End If
                borderW = value
                tb.RefreshInfoWithoutInvalidate()
                tb.Invalidate()
            End If
        End Set
    End Property
    Public Overrides Property Font As Font
        Get
            Return fnt
        End Get
        Set(value As Font)
            If Not fnt Is value Then
                fnt = value
                UpdateStyle()
                tb.RefreshInfoWithoutInvalidate()
                tb.Invalidate()
            End If
        End Set
    End Property
    Public Property SliderColor As Color
        Get
            Return slider_normal
        End Get
        Set(value As Color)
            If Not slider_normal = value Then
                slider_normal = value
                tb.Invalidate()
            End If
        End Set
    End Property
    Public Property SliderBackColor As Color
        Get
            Return slider_back
        End Get
        Set(value As Color)
            If Not slider_back = value Then
                slider_back = value
                tb.Invalidate()
            End If
        End Set
    End Property
    Public Property SliderOverColor As Color
        Get
            Return slider_hot
        End Get
        Set(value As Color)
            If Not slider_hot = value Then
                slider_hot = value
                tb.Invalidate()
            End If
        End Set
    End Property
    Public Property SliderPressedColor As Color
        Get
            Return slider_down
        End Get
        Set(value As Color)
            If Not slider_down = value Then
                slider_down = value
                tb.Invalidate()
            End If
        End Set
    End Property
    Public Property SliderBorderColor As Color
        Get
            Return slider_border
        End Get
        Set(value As Color)
            If Not slider_border = value Then
                slider_border = value
                tb.Invalidate()
            End If
        End Set
    End Property
#End Region
#Region "Text Style"
    Public Sub AddTextStyle(ByVal ts As TextStyle)
        TSS.Add(ts)
        tb.InvalidateStyle(ts)
    End Sub
    Public Sub OverrideTextStyle(ByVal ts As TextStyle)
        Dim tsa As TextStyle = Nothing
        Dim isSame As Boolean = False
        Dim sameIndex As Integer = -1
        Dim isIntersect As Boolean = False
        Dim intersectIndex As Integer = 0
        Dim boxSame As Boolean = False
        For i As Integer = 0 To TSS.Count - 1
            tsa = TSS(i)
            boxSame = False
            If ts.StyleTarget.HasFlag(StyleTarget.SelectedContents) Then
                If ts.BoxIndex.Count = tsa.BoxIndex.Count Then
                    boxSame = True
                    For Each i2 In ts.BoxIndex
                        If Not tsa.BoxIndex.Contains(i2) Then
                            boxSame = False
                            Exit For
                        End If
                    Next
                End If
            End If
            If ts.Position = tsa.Position And ts.Length = tsa.Length And ts.StyleTarget = tsa.StyleTarget And boxSame Then
                isSame = True
                sameIndex = i
            ElseIf ((ts.Position < (tsa.Position + tsa.Length)) AndAlso (tsa.Position < (ts.Position + ts.Length))) Then
                isIntersect = True
                intersectIndex = i
            End If
        Next
        ts.IsOverride = True
        If isSame Then
            If isIntersect And intersectIndex > sameIndex Then
                TSS.Add(ts)
            Else
                TSS(sameIndex) = ts
            End If
        Else
            TSS.Add(ts)
        End If

        tb.InvalidateStyle(ts)
    End Sub
    Public Function GetTextStyle(ByVal position As Long, ByVal boxIndex As Integer, ByVal unit As PointUnit) As BaseStyle
        Dim pStart As Long = 0
        Dim cpr As Integer = tb.GetCharsPerRow(Me.Box(boxIndex))
        Dim trans As ITransformer = Me.Box(boxIndex).trans

        If unit = PointUnit.Byte Then
            pStart = Math.Floor(position / col_count) * cpr
            Dim col As Integer = Math.Floor((position Mod col_count) / trans.LengthPerData) * (trans.CharsPerData + trans.Sparator)
            pStart += col
        Else
            pStart = position
        End If

        Dim SMBackRGB As Color ' = Helper.GetRGB(SM.bkClr)
        Dim SMTextRGB As Color ' = Helper.GetRGB(Helper.Blend(SM.txClr, SM.bkClr, SM.txClr.A / 255))
        Dim SMSelTextRGB As Color ' = Helper.GetRGB(Helper.Blend(SM.seltxClr, SM.bkClr, SM.seltxClr.A / 255))
        Dim SMSelBackRGB As Color ' = Helper.GetRGB(Helper.Blend(SM.selbkClr, SM.bkClr, SM.selbkClr.A / 255))
        Dim SMOvrTextRGB As Color ' = Helper.GetRGB(Helper.Blend(SM.ovrtxClr, SM.bkClr, SM.ovrtxClr.A / 255))
        Dim SMOvrBackRGB As Color '= Helper.GetRGB(Helper.Blend(SM.ovrbkClr, SM.bkClr, SM.ovrbkClr.A / 255))
        Dim SMOvrText2RGB As Color ' = Helper.GetRGB(Helper.Blend(SM.ovrtx2Clr, SM.bkClr, SM.ovrtx2Clr.A / 255))
        Dim SMOvrBack2RGB As Color ' = Helper.GetRGB(Helper.Blend(SM.ovrbk2Clr, SM.bkClr, SM.ovrbk2Clr.A / 255))
        Dim SMGrayTextRGB As Color ' = Helper.GetRGB(Helper.Blend(SM.GrayTextColor, SM.bkClr, SM.GrayTextColor.A / 255))
        Dim SMGrayBackRGB As Color = Color.Red
        Dim SMBackBlendRGB As Color '= Helper.GetRGB(Helper.Blend(SM.highLightLineColor, SM.bkClr, SM.highLightLineColor.A / 255))
        Dim rBackS As Color = Nothing
        Dim rSelS As Color = Nothing
        Dim rFontS As Font = Nothing

        Dim box As BoxItem = Me.Box(boxIndex)

        If IsNothing(box.Style) Then
            SMBackRGB = bkClr
            SMTextRGB = Helper.Blend(txClr, bkClr, txClr.A / 255)
            SMSelTextRGB = Helper.Blend(seltxClr, bkClr, seltxClr.A / 255)
            SMSelBackRGB = Helper.Blend(selbkClr, bkClr, selbkClr.A / 255)
            SMOvrTextRGB = Helper.Blend(ovrtxClr, bkClr, ovrtxClr.A / 255)
            SMOvrBackRGB = Helper.Blend(ovrbkClr, bkClr, ovrbkClr.A / 255)
            SMOvrText2RGB = Helper.Blend(ovrtx2Clr, bkClr, ovrtx2Clr.A / 255)
            SMOvrBack2RGB = Helper.Blend(ovrbk2Clr, bkClr, ovrbk2Clr.A / 255)
            SMGrayTextRGB = Helper.Blend(graytxClr, bkClr, graytxClr.A / 255)
            SMBackBlendRGB = Helper.Blend(highLightLineColor, bkClr, highLightLineColor.A / 255)
            rBackS = bkClr
            rSelS = selbkClr
            rFontS = Font
        Else
            Dim stsSource As BaseStyle = box.Style
            Dim sts As New BaseStyle
            If stsSource.BackColor.IsEmpty Then
                sts.BackColor = bkClr
            Else
                sts.BackColor = stsSource.BackColor
            End If

            If stsSource.TextColor.IsEmpty Then
                sts.TextColor = txClr
            Else
                sts.TextColor = stsSource.TextColor
            End If

            If stsSource.HightlightTextColor.IsEmpty Then
                sts.HightlightTextColor = seltxClr
            Else
                sts.HightlightTextColor = stsSource.HightlightTextColor
            End If

            If stsSource.HightlightBackColor.IsEmpty Then
                sts.HightlightBackColor = selbkClr
            Else
                sts.HightlightBackColor = stsSource.HightlightBackColor
            End If

            If stsSource.HotBackColor.IsEmpty Then
                sts.HotBackColor = ovrbkClr
            Else
                sts.HotBackColor = stsSource.HotBackColor
            End If

            If stsSource.HotTextColor.IsEmpty Then
                sts.HotTextColor = ovrtxClr
            Else
                sts.HotTextColor = stsSource.HotTextColor
            End If

            If stsSource.HotBackColor2.IsEmpty Then
                sts.HotBackColor2 = ovrbk2Clr
            Else
                sts.HotBackColor2 = stsSource.HotBackColor2
            End If

            If stsSource.HotTextColor2.IsEmpty Then
                sts.HotTextColor2 = ovrtx2Clr
            Else
                sts.HotTextColor2 = stsSource.HotTextColor2
            End If

            If stsSource.GrayTextColor.IsEmpty Then
                sts.GrayTextColor = graytxClr
            Else
                sts.GrayTextColor = stsSource.GrayTextColor
            End If
            If IsNothing(stsSource.Font) Then
                sts.Font = Font
            Else
                sts.Font = stsSource.Font
            End If
            rFontS = sts.Font

            SMBackRGB = sts.BackColor
            SMTextRGB = Helper.Blend(sts.TextColor, sts.BackColor, sts.TextColor.A / 255)
            SMSelTextRGB = Helper.Blend(sts.HightlightTextColor, sts.BackColor, sts.HightlightTextColor.A / 255)
            SMSelBackRGB = Helper.Blend(sts.HightlightBackColor, sts.BackColor, sts.HightlightBackColor.A / 255)
            SMOvrTextRGB = Helper.Blend(sts.HotTextColor, sts.BackColor, sts.HotTextColor.A / 255)
            SMOvrBackRGB = Helper.Blend(sts.HotBackColor, sts.BackColor, sts.HotBackColor.A / 255)
            SMOvrText2RGB = Helper.Blend(sts.HotTextColor2, sts.BackColor, sts.HotTextColor2.A / 255)
            SMOvrBack2RGB = Helper.Blend(sts.HotBackColor2, sts.BackColor, sts.HotBackColor2.A / 255)
            SMGrayTextRGB = Helper.Blend(sts.GrayTextColor, sts.BackColor, sts.GrayTextColor.A / 255)
            SMBackBlendRGB = Helper.Blend(highLightLineColor, sts.BackColor, highLightLineColor.A / 255)
            rBackS = sts.BackColor
            rSelS = sts.HightlightBackColor
        End If

        Dim tsRes As New BaseStyle
        tsRes.TextColor = Me.txClr
        tsRes.BackColor = Me.bkClr
        tsRes.HightlightTextColor = Me.seltxClr
        tsRes.HightlightBackColor = Me.selbkClr
        tsRes.HotTextColor = Me.ovrtxClr
        tsRes.HotBackColor = Me.ovrbkClr
        tsRes.HotTextColor2 = Me.ovrtx2Clr
        tsRes.HotBackColor2 = Me.ovrbk2Clr
        tsRes.GrayTextColor = Me.graytxClr
        tsRes.Font = Me.fnt

        For Each i In TSS
            If i.StyleTarget.HasFlag(StyleTarget.AllContents) Or (i.StyleTarget.HasFlag(StyleTarget.SelectedContents) And i.BoxIndex.Contains(boxIndex)) Then
                Dim tsStart As Long = 0
                Dim tsEnd As Long = 0
                If i.Unit = PointUnit.Byte Then
                    tsStart = Math.Floor(i.Position / col_count) * cpr
                    Dim col As Integer = Math.Floor((i.Position Mod col_count) / trans.LengthPerData) * (trans.CharsPerData + trans.Sparator)
                    pStart += col

                    tsEnd = Math.Floor((i.Position + i.Length) / col_count) * cpr
                    col = Math.Floor(((i.Position + i.Length) Mod col_count) / trans.LengthPerData) * (trans.CharsPerData + trans.Sparator)
                    tsEnd += col
                Else
                    tsStart = i.Position
                    tsEnd = i.Position + i.Length
                End If
                If tsStart <= pStart And pStart < tsEnd Then
                    If Not i.TextColor.IsEmpty Then
                        tsRes.TextColor = i.TextColor
                    ElseIf i.IsOverride Then
                        tsRes.TextColor = SMTextRGB
                    End If
                    If Not i.BackColor.IsEmpty Then
                        tsRes.BackColor = i.BackColor
                    ElseIf i.IsOverride Then
                        tsRes.BackColor = SMBackRGB
                    End If
                    If Not i.HightlightTextColor.IsEmpty Then
                        tsRes.HightlightTextColor = i.HightlightTextColor
                    ElseIf i.IsOverride Then
                        tsRes.HightlightTextColor = SMSelTextRGB
                    End If
                    If Not i.HightlightBackColor.IsEmpty Then
                        tsRes.HightlightBackColor = i.HightlightBackColor
                    ElseIf i.IsOverride Then
                        tsRes.HightlightBackColor = SMSelBackRGB
                    End If
                    If Not i.HotTextColor.IsEmpty Then
                        tsRes.HotTextColor = i.HotTextColor
                    ElseIf i.IsOverride Then
                        tsRes.HotTextColor = SMOvrTextRGB
                    End If
                    If Not i.HotBackColor.IsEmpty Then
                        tsRes.HotBackColor = i.HotBackColor
                    ElseIf i.IsOverride Then
                        tsRes.HotBackColor = SMOvrBackRGB
                    End If
                    If Not i.HotTextColor2.IsEmpty Then
                        tsRes.HotTextColor2 = i.HotTextColor2
                    ElseIf i.IsOverride Then
                        tsRes.HotTextColor2 = SMOvrText2RGB
                    End If
                    If Not i.HotBackColor2.IsEmpty Then
                        tsRes.HotBackColor2 = i.HotBackColor2
                    ElseIf i.IsOverride Then
                        tsRes.HotBackColor2 = SMOvrBack2RGB
                    End If
                    If Not i.GrayTextColor.IsEmpty Then
                        tsRes.GrayTextColor = i.GrayTextColor
                    ElseIf i.IsOverride Then
                        tsRes.GrayTextColor = SMGrayTextRGB
                    End If
                    If Not IsNothing(i.Font) Then
                        tsRes.Font = i.Font
                    ElseIf i.IsOverride Then
                        tsRes.Font = rFontS
                    End If
                End If
            End If
        Next

        Return tsRes
    End Function
    Public Sub ClearTextStyles()
        TSS.Clear()
        tb.Invalidate()
    End Sub
    Public Function GetTextStyles() As TextStyle()
        Return TSS.ToArray
    End Function
    Public Sub ClearTextStyle(ByVal position As Long, ByVal length As Long, ByVal unit As PointUnit, ByVal target As StyleTarget, ByVal boxIndex As Integer())
        Dim ts As New TextStyle
        ts.Position = position
        ts.Length = length
        ts.Unit = unit
        ts.StyleTarget = target
        If Not IsNothing(boxIndex) Then
            If Not boxIndex.Count = 0 Then
                ts.BoxIndex.AddRange(boxIndex)
            End If
        End If
        ts.IsOverride = True

        Dim tsa As TextStyle = Nothing
        Dim isSame As Boolean = False
        Dim sameIndex As Integer = -1
        Dim isIntersect As Boolean = False
        Dim intersectIndex As Integer = 0
        Dim boxSame As Boolean = False
        For i As Integer = 0 To TSS.Count - 1
            tsa = TSS(i)
            boxSame = False
            If ts.StyleTarget.HasFlag(StyleTarget.SelectedContents) Then
                If ts.BoxIndex.Count = tsa.BoxIndex.Count Then
                    boxSame = True
                    For Each i2 In ts.BoxIndex
                        If Not tsa.BoxIndex.Contains(i2) Then
                            boxSame = False
                            Exit For
                        End If
                    Next
                End If
            End If
            If ts.Position = tsa.Position And ts.Length = tsa.Length And ts.StyleTarget = tsa.StyleTarget And boxSame Then
                isSame = True
                sameIndex = i
            ElseIf ((ts.Position < (tsa.Position + tsa.Length)) AndAlso (tsa.Position < (ts.Position + ts.Length))) Then
                isIntersect = True
                intersectIndex = i
            End If
        Next

        If isSame Then
            If isIntersect And intersectIndex > sameIndex Then
                TSS.Add(ts)
            Else
                TSS(sameIndex) = ts
            End If
        ElseIf isIntersect Then
            TSS.Add(ts)
        End If

        tb.InvalidateStyle(ts)
    End Sub

    Public Sub SetBoxStyle(ByVal boxIndex As Integer, ByVal ts As BaseStyle)
        Box(boxIndex).Style = ts
    End Sub
    Public Function GetBoxStyle(ByVal boxIndex As Integer) As BaseStyle
        Return Box(boxIndex).Style
    End Function
    Public Function GetBoxStyle(ByVal box As BoxItem) As BaseStyle
        Return box.Style
    End Function
    Public Sub SetBoxStyle(ByVal box As BoxItem, ByVal ts As BaseStyle)
        box.Style = ts
    End Sub
#End Region
End Class

Public Class BoxItem
    Friend w As Integer
    Friend trans As ITransformer
    Friend xpad As Integer = 20
    Friend ix As Integer
    Friend tb As HexBox
    Friend ts As BaseStyle
    Friend az As Boolean = True
    Public Property Style As BaseStyle
        Get
            Return ts
        End Get
        Set(value As BaseStyle)
            Dim oldFont As Font = Nothing
            Dim newFont As Font = Nothing
            If Not IsNothing(ts) Then oldFont = ts.Font
            ts = value
            If Not IsNothing(value) Then
                If Not IsNothing(value.Font) Then
                    newFont = value.Font
                End If
            End If

            If Not newFont Is oldFont Then
                tb.SM.UpdateBox(Me)
            End If
            tb.Invalidate()
        End Set
    End Property
    Public Property Mode As TransformMode
        Get
            Return trans.Mode
        End Get
        Set(value As TransformMode)
            trans = Transformers.Create(value)
            If tb.FocussedBoxIndex = Me.Index Then
                tb.SL = tb.SL.CreateTransform(trans)
            End If
            tb.SM.UpdateBox(Me)
            tb.Invalidate()
            tb.Update()
        End Set
    End Property
    Public Property Transformer As ITransformer
        Get
            Return trans
        End Get
        Set(value As ITransformer)
            trans = value
            If tb.FocussedBoxIndex = Me.Index Then
                tb.SL = tb.SL.CreateTransform(trans)
            End If
            tb.SM.UpdateBox(Me)
            tb.Invalidate()
            tb.Update()
        End Set
    End Property
    Public ReadOnly Property CharsLengthPerRow As Integer
        Get
            Return tb.GetCharsPerRow(Me)
        End Get
    End Property
    Public Property Index As Integer
        Get
            Return tb.SM.Box.IndexOf(Me)
        End Get
        Set(value As Integer)
            Dim oldIndex As Integer = tb.SM.Box.IndexOf(Me)
            If Not value = oldIndex Then
                tb.SM.Box.Remove(Me)
                Dim c As Integer = tb.SM.Box.Count
                If value >= c Then
                    tb.SM.Box.Add(Me)
                Else
                    tb.SM.Box.Insert(value, Me)
                End If

                If tb.FocussedBoxIndex = oldIndex Then
                    tb.SL = tb.SL.CreateTransform(tb.SelectedBox.trans)
                End If

                tb.Invalidate()
                tb.Update()
            End If
        End Set
    End Property
    Public Property Width As Integer
        Get
            Return w
        End Get
        Set(value As Integer)
            If Not az Then
                w = value
                tb.Invalidate()
                tb.Update()
            End If
        End Set
    End Property
    Public Property PaddingLeft As Integer
        Get
            Return xpad
        End Get
        Set(value As Integer)
            If Not value = xpad Then
                xpad = value
                tb.Invalidate()
                tb.Update()
            End If
        End Set
    End Property
    Public Property AutoSize As Boolean
        Get
            Return az
        End Get
        Set(value As Boolean)
            az = value
            If az Then
                tb.SM.UpdateBox(Me)
                tb.Invalidate()
                tb.Update()
            End If
        End Set
    End Property
    Friend Sub New(ByVal tb As HexBox)
        Me.tb = tb
    End Sub
    Friend Sub New(ByVal tb As HexBox, ByVal mode As TransformMode)
        Me.trans = Transformers.Create(mode)
        Me.tb = tb
    End Sub
    Friend Sub New(ByVal tb As HexBox, ByVal trans As ITransformer)
        Me.trans = trans
        Me.tb = tb
    End Sub

End Class

Friend Class FontData
    Public Font As Font
    Public ABC As GDI32.AbcFloat()
    Public hFont As IntPtr
    Friend Sub New()

    End Sub
    Public Sub New(ByVal font As Font)
        Me.Font = font
        Me.hFont = font.ToHfont
        Me.ABC = Helper.GetABCWidthsW(hFont)
    End Sub
End Class
Public Class BaseStyle
    Public Overridable Property TextColor As Color
    Public Overridable Property BackColor As Color
    Public Overridable Property HightlightTextColor As Color
    Public Overridable Property HightlightBackColor As Color
    Public Overridable Property HotTextColor As Color
    Public Overridable Property HotBackColor As Color
    Public Overridable Property HotTextColor2 As Color
    Public Overridable Property HotBackColor2 As Color
    Public Overridable Property HotTextColor3 As Color
    Public Overridable Property HotBackColor3 As Color
    Public Overridable Property GrayTextColor As Color
    Public Overridable Property WrongTextColor As Color
    Public Overridable Property UnAccessableTextColor As Color
    Public Overridable Property UnEditableTextColor As Color
    Public Overridable Property Font As Font
End Class
Public Class TextStyle
    Inherits BaseStyle

    Public Property Position As Long
    Public Property Length As Long
    Public Property StyleTarget As StyleTarget
    Public Property Unit As PointUnit
    Public Property BoxIndex As New List(Of Integer)
    Public Property IsAutoStyle As Boolean
    Public Property IsOverride As Boolean
    Public Function Contains(ByVal index As Long) As Boolean
        Dim str As Long = Position
        Dim snd As Long = Position + Length

        If str <= index And index < snd Then
            Return True
        Else
            Return False
        End If
    End Function
    Public Function Contains(ByVal index As Long, source As BoxItem, dest As BoxItem) As Boolean
        Dim item As New SelectionItem
        item.anc = Position
        item.car = Position + Length

        item = source.tb.SL.TransformItem(item, source.trans, dest.trans)

        Dim str As Long = item.anc
        Dim snd As Long = item.car

        If str <= index And index < snd Then
            Return True
        Else
            Return False
        End If
    End Function
    Public Sub Invalidate(ByVal tb As HexBox)
        tb.InvalidateStyle(Me)
    End Sub
End Class

<Browsable(True)>
Public Enum HeaderMode As Integer
    Hidden = 0
    [Default] = 1
    Dynamic = 2
End Enum

<Browsable(True)>
Public Enum OffsetMode As Integer
    Hidden = 0
    Hex1 = 2
    Hex2 = 4
    Hex4 = 8
    Hex6 = 12
    Hex8 = 16
    Bytes1 = 17
    Bytes2 = 18
    Bytes4 = 19
    Bytes8 = 20
End Enum

<Browsable(True)>
Public Enum WriteMode As Integer
    Insert = 0
    Overwrite = 1
End Enum

<Browsable(True)>
Public Enum StyleTarget As Integer
    Offset = 1
    CollumnHeader = 2
    SelectedContents = 4
    AllContents = 8
    All = 16
End Enum

<Browsable(True)>
Public Enum PointUnit As Integer
    [Byte] = 0
    [Char] = 1
End Enum
<Browsable(True)>
Public Enum OverModes As Integer
    Line = 1
    Color = 2
End Enum