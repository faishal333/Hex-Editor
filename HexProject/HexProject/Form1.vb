Imports System.IO
Imports System.Runtime.InteropServices
Imports System.Text
Imports SalSal

Public Class Form1
    Dim ansi As System.Text.Encoding = System.Text.Encoding.Default
    Dim ts As New BaseStyle
    Dim ts2 As New BaseStyle

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        CheckBox1.Checked = HexBox1.AutoSnap
        CheckBox2.Checked = HexBox1.ShowBorder
        CheckBox3.Checked = Not HexBox1.CollumnHeaderMode = SalSal.HeaderMode.Hidden
        CheckBox4.Checked = Not HexBox1.OffsetType = SalSal.OffsetMode.Hidden
        CheckBox5.Checked = HexBox1.HighlightCurrentLine

        ComboBox1.SelectedItem = "Hex"
        ComboBox2.SelectedItem = "Hex"
        ComboBox3.SelectedItem = "16"

        HexBox1.ExtraWidth = Panel1.Width + Panel2.Width
        'TransformBox1.Styles.BackColor = SystemColors.Control

        Dim clr As Color = HexBox1.Styles.HighlightBackColor
        ts.HightlightBackColor = Color.DimGray
        For Each i In HexBox1.BoxItems
            i.Style = ts
        Next
        HexBox1.SelectedBox.Style = ts2

        HexBox1.ShowInvalidateArea = False
        HexBox1.MultiSelection = True
        'HexBox1.Collumn = 16
        LoadFile("C:\Users\Faishal\Documents\Visual Studio 2015\Projects\HexProject\HexProject\bin\Debug\test.txt")
        Button4.PerformClick()
    End Sub

    Private Sub CheckBox1_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBox1.CheckedChanged
        HexBox1.AutoSnap = CheckBox1.Checked
    End Sub

    Private Sub CheckBox2_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBox2.CheckedChanged
        HexBox1.ShowBorder = CheckBox2.Checked
        HexBox1.FullHighLightLine = Not CheckBox2.Checked
    End Sub

    Private Sub CheckBox3_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBox3.CheckedChanged
        If CheckBox3.Checked Then
            HexBox1.CollumnHeaderMode = SalSal.HeaderMode.Default
        Else
            HexBox1.CollumnHeaderMode = SalSal.HeaderMode.Hidden
        End If
    End Sub

    Private Sub CheckBox4_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBox4.CheckedChanged
        If CheckBox4.Checked Then
            HexBox1.OffsetType = SalSal.OffsetMode.Hex2
        Else
            HexBox1.OffsetType = SalSal.OffsetMode.Hidden
        End If
    End Sub

    Private Sub CheckBox5_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBox5.CheckedChanged
        HexBox1.HighlightCurrentLine = CheckBox5.Checked
    End Sub

    Private Sub ComboBox1_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ComboBox1.SelectedIndexChanged
        Select Case ComboBox1.SelectedItem
            Case "Hex"
                HexBox1.ViewMode = SalSal.TransformMode.HexView
            Case "Hex - 2 Bytes"
                HexBox1.ViewMode = SalSal.TransformMode.Hex2View
            Case "Hex - 4 Bytes"
                HexBox1.ViewMode = SalSal.TransformMode.Hex4View
            Case "Hex - 8 Bytes"
                HexBox1.ViewMode = SalSal.TransformMode.Hex8View
            Case "Byte"
                HexBox1.ViewMode = SalSal.TransformMode.ByteView
            Case "Binary"
                HexBox1.ViewMode = SalSal.TransformMode.BinaryView
            Case "Int16"
                HexBox1.ViewMode = SalSal.TransformMode.Int16View
            Case "Int32"
                HexBox1.ViewMode = SalSal.TransformMode.Int32View
            Case "Int64"
                HexBox1.ViewMode = SalSal.TransformMode.Int64View
            Case "UInt16"
                HexBox1.ViewMode = SalSal.TransformMode.UInt16View
            Case "UInt32"
                HexBox1.ViewMode = SalSal.TransformMode.UInt32View
            Case "UInt64"
                HexBox1.ViewMode = SalSal.TransformMode.UInt64View
            Case "Float"
                HexBox1.ViewMode = SalSal.TransformMode.FloatView
            Case "Double"
                HexBox1.ViewMode = SalSal.TransformMode.DoubleView
            Case "Char (ANSI)"
                HexBox1.ViewMode = SalSal.TransformMode.CharView
            Case "Char (Unicode)"
                HexBox1.ViewMode = SalSal.TransformMode.UnicodeView
            Case "Text"
                HexBox1.ViewMode = SalSal.TransformMode.TextView
            Case "TextW"
                HexBox1.ViewMode = SalSal.TransformMode.TextWView
        End Select
    End Sub

    Private Sub ComboBox2_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ComboBox2.SelectedIndexChanged
        Select Case ComboBox2.SelectedItem
            Case "Hex"
                HexBox1.OffsetType = SalSal.OffsetMode.Hex4
            Case "Decimal"
                HexBox1.OffsetType = SalSal.OffsetMode.Bytes4
        End Select
    End Sub

    Private Sub ComboBox3_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ComboBox3.SelectedIndexChanged
        Select Case ComboBox3.SelectedItem
            Case "2"
                HexBox1.Collumn = 2
            Case "4"
                HexBox1.Collumn = 4
            Case "8"
                HexBox1.Collumn = 8
            Case "16"
                HexBox1.Collumn = 16
            Case "32"
                HexBox1.Collumn = 32
        End Select
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Dim fod As New OpenFileDialog()
        fod.Filter = "All Files|*.*"
        If fod.ShowDialog(Me) = DialogResult.OK Then
            LoadFile(fod.FileName)
        End If
    End Sub
    Friend Sub LoadFile(ByVal path As String)
        HexBox1.LoadFile(path)
        TextBox1.Text = path
        Label6.Text = SalSal.Helper.GetSizeText(HexBox1.ByteBuilder.Length)
        RefreshStatus()
    End Sub

    Private Sub Timer1_Tick(sender As Object, e As EventArgs) Handles Timer1.Tick

    End Sub
    Dim hf As IntPtr = 0
    Private Sub TransformBox1_GDI32Paint(sender As Object, e As GDI32PaintEventArgs) Handles HexBox1.GDI32Paint
        If CheckBox6.Checked Then
            If hf = 0 Then
                hf = HexBox1.Styles.Font.ToHfont
            End If

            Dim bw As Integer = 0
            For Each box In HexBox1.BoxItems
                Dim trans As ITransformer = box.Transformer
                Dim x As Integer = HexBox1.OffsetWidth + box.PaddingLeft + e.Translate.X + bw
                x -= Math.Floor(Helper.GetTextWidth(hf, "".PadRight(trans.Sparator)) / 2)
                Dim c As Integer = HexBox1.Collumn - 1
                For i As Integer = 0 To c - 4 Step 4
                    Dim w As Single = Helper.GetTextWidth(hf, "".PadRight((trans.CharsPerData + trans.Sparator) / trans.LengthPerData * 4))
                    Helper.FillRectangle(e.hDC, New Rectangle(x + w, 0, 1, Me.Height), Color.Yellow)
                    x += w
                Next
                bw += box.Width
            Next
        End If
    End Sub

    Private Sub ToolStripStatusLabel1_Click(sender As Object, e As EventArgs) Handles ToolStripStatusLabel1.Click
        If HexBox1.WriteMode = WriteMode.Overwrite Then
            HexBox1.WriteMode = WriteMode.Insert
            ToolStripStatusLabel1.Text = "INS"
        Else
            HexBox1.WriteMode = WriteMode.Overwrite
            ToolStripStatusLabel1.Text = "OVR"
        End If
    End Sub

    Dim status1Hex As Boolean
    Dim status2Hex As Boolean
    Public Sub RefreshStatus()
        Dim box As BoxItem = HexBox1.SelectedBox
        Dim fileSize As Long = HexBox1.ByteBuilder.GetLength
        Dim fileName As String = "Ready"
        Dim fs As IO.FileStream = TryCast(HexBox1.ByteBuilder.BaseStream, IO.FileStream)
        If Not IsNothing(fs) Then
            fileName = IO.Path.GetFileName(fs.Name)
        End If
        Dim caret As Long = HexBox1.Selection.Curent.Caret
        Dim cpr As Integer = box.CharsLengthPerRow
        Dim lines As Long = Math.Floor(fileSize / HexBox1.Collumn)
        Dim curentLine As Long = Math.Floor(caret / cpr)
        Dim col As Integer = Math.Floor((caret Mod cpr) / (box.Transformer.CharsPerData + box.Transformer.Sparator))
        Dim sel As Long = HexBox1.Selection.SelectionLength
        Dim selb As Long = Math.Floor(HexBox1.Selection.SelectionLength / cpr) * HexBox1.Collumn
        Dim selc As Integer = Math.Ceiling((HexBox1.Selection.SelectionLength Mod cpr) / (box.Transformer.CharsPerData + box.Transformer.Sparator)) * box.Transformer.LengthPerData
        If selc > HexBox1.Collumn Then selc = HexBox1.Collumn
        selb += selc

        ToolStripStatusLabel3.Text = fileName
        If status1Hex Then
            ToolStripStatusLabel4.Text = "length : " & Hex(fileSize) & "h (" & Helper.GetSizeText(fileSize) & ")   " & vbTab & "lines : " & Hex(lines) & "h"
        Else
            ToolStripStatusLabel4.Text = "length : " & fileSize & " (" & Helper.GetSizeText(fileSize) & ")   " & vbTab & "lines : " & lines
        End If

        If status2Hex Then
            ToolStripStatusLabel5.Text = "Ln : " & Hex(curentLine) & "h   Col : " & Hex(col) & "h   Sel : " & Hex(sel) & "h   Bytes : " & Hex(selb) & "h"
        Else
            ToolStripStatusLabel5.Text = "Ln : " & curentLine & "   Col : " & col & "   Sel : " & sel & "   Bytes : " & selb
        End If
    End Sub

    Private Sub ToolStripStatusLabel4_Click(sender As Object, e As EventArgs) Handles ToolStripStatusLabel4.Click
        status1Hex = Not status1Hex
        RefreshStatus()
    End Sub
    Private Sub ToolStripStatusLabel5_Click(sender As Object, e As EventArgs) Handles ToolStripStatusLabel5.Click
        status2Hex = Not status2Hex
        RefreshStatus()
    End Sub
    Private Sub TransformBox1_SelectionChanged(sender As Object, e As SalSal.HexBoxSelectionEventArgs) Handles HexBox1.SelectionChanged
        RefreshStatus()
        If Not e.OldBoxIndex = e.NewBoxIndex Then
            For Each i In HexBox1.BoxItems
                i.Style = ts
            Next
            HexBox1.BoxItems(e.NewBoxIndex).Style = ts2
        End If
        Dim tsx As BaseStyle = HexBox1.Styles.GetTextStyle(e.NewSelection.Curent.Caret, HexBox1.SelectedBoxIndex, PointUnit.Char)
        Dim textColor As Color = HexBox1.Styles.TextColor
        Dim backColor As Color = HexBox1.Styles.BackColor
        If Not IsNothing(HexBox1.SelectedBox.Style) Then
            textColor = HexBox1.SelectedBox.Style.TextColor
            backColor = HexBox1.SelectedBox.Style.BackColor

            If textColor.IsEmpty Then
                textColor = HexBox1.Styles.TextColor
            End If
            If backColor.IsEmpty Then
                backColor = HexBox1.Styles.BackColor
            End If
        End If
        If Not IsNothing(tsx) Then
            If Not tsx.TextColor.IsEmpty Then
                textColor = tsx.TextColor
            End If
            If Not tsx.BackColor.IsEmpty Then
                backColor = tsx.BackColor
            End If
        End If
        Button7.BackColor = textColor
        Button6.BackColor = backColor

    End Sub
    Dim colaps As Boolean
    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button4.Click
        If colaps Then
            Button4.Text = ">"
            Panel1.Visible = True
            HexBox1.ExtraWidth = Panel1.Width + Panel2.Width
        Else
            Button4.Text = "<"
            Panel1.Visible = False
            HexBox1.ExtraWidth = Panel2.Width
        End If

        colaps = Not colaps
    End Sub

    Private Sub Button4_GotFocus(sender As Object, e As EventArgs) Handles Button4.GotFocus
        CheckBox1.Focus()
    End Sub
    Private Sub Button4_Enter(sender As Object, e As EventArgs) Handles Button4.Enter
        CheckBox1.Focus()
    End Sub

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        Dim s As StyleSet = HexBox1.Styles
        s.BackColor = SystemColors.Window
        s.TextColor = SystemColors.WindowText
        s.HighlightBackColor = SystemColors.Highlight
        s.HighlightTextColor = SystemColors.Window
        s.HighlighRowColor = Color.FromArgb(150, Color.LightGray)

        s.HotBackColor = Color.FromArgb(150, Color.Yellow)
        s.HotTextColor = SystemColors.WindowText
        s.HotBackColor2 = Color.LightBlue
        s.HotTextColor2 = SystemColors.WindowText
        s.HotBackColor3 = Color.FromArgb(150, Color.White)
        s.HotTextColor3 = Color.Red

        s.OffsetBackColor = SystemColors.Control
        s.OffsetTextColor = SystemColors.WindowText
        s.CollumnHeaderBackColor = s.OffsetBackColor
        s.CollumnHeaderTextColor = s.OffsetTextColor

        s.BorderColor = SystemColors.ActiveBorder

        s.SliderBorderColor = SystemColors.ScrollBar
        s.SliderPressedColor = SystemColors.ControlDark


        Dim clr As Color = HexBox1.Styles.HighlightBackColor
        ts.HightlightBackColor = Color.DimGray
        For Each i In HexBox1.BoxItems
            i.Style = ts
        Next
        HexBox1.SelectedBox.Style = Nothing
    End Sub
    Private Sub Button5_Click(sender As Object, e As EventArgs) Handles Button5.Click
        Dim s As StyleSet = HexBox1.Styles
        s.BackColor = Color.FromArgb(&HFF1E1E1E)
        s.TextColor = Color.FromArgb(&HFFDCDCDC)
        s.HighlightBackColor = Color.FromArgb(150, SystemColors.Highlight) 'Color.FromArgb(&HFF264F78)
        s.HighlightTextColor = s.TextColor
        s.HighlighRowColor = Color.FromArgb(&HFF0F0F0F)

        s.HotBackColor = Color.FromArgb(&HFF3D6C0E)
        s.HotTextColor = s.TextColor
        s.HotBackColor2 = Color.FromArgb(&HFF686868)
        s.HotTextColor2 = s.TextColor
        s.HotBackColor3 = Color.FromArgb(30, Color.White)
        s.HotTextColor3 = Color.Orange

        s.OffsetBackColor = Color.FromArgb(&HFF333333)
        s.OffsetTextColor = Color.DodgerBlue
        s.CollumnHeaderBackColor = s.OffsetBackColor
        s.CollumnHeaderTextColor = s.OffsetTextColor

        s.BorderColor = Color.FromArgb(&HFF626262)

        s.SliderBorderColor = s.BorderColor
        s.SliderPressedColor = s.OffsetTextColor

        Dim clr As Color = HexBox1.Styles.HighlightBackColor
        ts.HightlightBackColor = Color.FromArgb(clr.A, Helper.Blend(Color.Red, clr, 0.5))
        For Each i In HexBox1.BoxItems
            i.Style = ts
        Next
        HexBox1.SelectedBox.Style = Nothing
    End Sub

    Private Sub Button7_Click(sender As Object, e As EventArgs) Handles Button7.Click
        Dim sl As SelectionManager = HexBox1.Selection

        Dim cd As New ColorDialog
        cd.Color = Button7.BackColor
        If cd.ShowDialog(Me) = DialogResult.OK Then
            Button7.BackColor = cd.Color

            For Each i In sl.Items
                Dim ts As New TextStyle
                ts.TextColor = cd.Color
                ts.Position = i.SelectionStart
                ts.Length = i.SelectionLength
                ts.Unit = PointUnit.Char
                ts.StyleTarget = StyleTarget.SelectedContents
                ts.BoxIndex.Add(HexBox1.SelectedBoxIndex)
                HexBox1.Styles.AddTextStyle(ts)
            Next
        End If
    End Sub

    Private Sub Button6_Click(sender As Object, e As EventArgs) Handles Button6.Click
        Dim sl As SelectionManager = HexBox1.Selection

        Dim cd As New ColorDialog
        cd.Color = Button6.BackColor
        If cd.ShowDialog(Me) = DialogResult.OK Then
            Button6.BackColor = cd.Color

            For Each i In sl.Items
                Dim ts As New TextStyle
                ts.BackColor = Color.FromArgb(150, cd.Color)
                ts.Position = i.SelectionStart
                ts.Length = i.SelectionLength
                ts.Unit = PointUnit.Char
                ts.StyleTarget = StyleTarget.SelectedContents
                ts.BoxIndex.Add(HexBox1.SelectedBoxIndex)
                HexBox1.Styles.AddTextStyle(ts)
            Next
        End If
    End Sub

    Private Sub Button8_Click(sender As Object, e As EventArgs) Handles Button8.Click
        Dim cd As New FontDialog
        cd.Font = HexBox1.Styles.Font
        If cd.ShowDialog(Me) = DialogResult.OK Then
            HexBox1.Styles.Font = cd.Font
        End If
    End Sub

    Private Sub Button9_Click(sender As Object, e As EventArgs) Handles Button9.Click
        Dim sl As SelectionManager = HexBox1.Selection
        For Each i In sl.Items
            HexBox1.Styles.ClearTextStyle(i.SelectionStart, i.SelectionLength, PointUnit.Char, StyleTarget.SelectedContents, {HexBox1.SelectedBoxIndex})
        Next

    End Sub

    Private Sub CheckBox6_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBox6.CheckedChanged
        HexBox1.Invalidate()
    End Sub

    Private Sub CheckBox7_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBox7.CheckedChanged
        HexBox1.ForceHotFind = CheckBox7.Checked
    End Sub

    Private Sub CheckBox8_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBox8.CheckedChanged
        HexBox1.ShowShiftSlider = CheckBox8.Checked
    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        HexBox1.ByteBuilder.Flush()
    End Sub
End Class
