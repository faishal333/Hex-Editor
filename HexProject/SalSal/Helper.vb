Imports System.Drawing
Imports System.Windows.Forms

Public Class Helper
    Friend Shared fakeBmp As New Bitmap(1, 1)
    Friend Shared fakeGraphics As Graphics = Graphics.FromImage(fakeBmp)
    Friend Shared fakeGraphics2 As Graphics = Graphics.FromImage(fakeBmp)
    Friend Shared hDC As IntPtr = fakeGraphics.GetHdc()
    Friend Shared ansi As System.Text.Encoding = System.Text.Encoding.Default
    Public Shared Function IntersectsWith(a1 As Long, a2 As Long, b1 As Long, b2 As Long) As Boolean
        Return (b1 < a2) And (a1 < b2)
    End Function


    Public Shared Function Hex2(ByVal n As Long) As String
        Dim s As String = Hex(n).PadLeft(2, "0")
        Return Mid(s, s.Length - 1)
    End Function
    Public Shared Function Hex4(ByVal n As Long) As String
        Dim s As String = Hex(n).PadLeft(4, "0")
        Return Mid(s, s.Length - 3)
    End Function
    Public Shared Function Hex8(ByVal n As Long) As String
        Dim s As String = Hex(n).PadLeft(8, "0")
        Return Mid(s, s.Length - 7)
    End Function
    Public Shared Function HexN(ByVal n As Long, ByVal c As Integer) As String
        Dim s As String = Hex(n).PadLeft(c, "0")
        Return Mid(s, s.Length - (c - 1))
    End Function
    Public Shared Function HexFlip(ByVal s As String) As String
        Dim rs As String = ""
        For i As Integer = s.Length - 1 To 1 Step -2
            rs &= Mid(s, i, 2)
        Next
        If s.Length Mod 2 = 1 Then
            rs &= Mid(s, 1, 1)
        End If
        Return rs
    End Function
    Friend Shared bbin As Byte() = {128, 64, 32, 16, 8, 4, 2, 1}
    Public Shared Function Binary(ByVal v As Byte) As String
        Dim c(7) As Char
        Dim b As Byte
        For i As Integer = 0 To 7
            b = bbin(i)
            If v >= b Then
                c(i) = "1"
                v -= b
            Else
                c(i) = "0"
            End If
        Next
        Return c
    End Function
    Public Shared Function BinaryToByte(ByVal v As String) As Byte
        v = v.PadLeft(8, "0")
        v = Mid(v, v.Length - 7)
        Dim b As Byte = 0
        For i As Integer = 7 To 0 Step -1
            If v(i) = "1" Then
                b += bbin(i)
            End If
        Next
        Return b
    End Function
    Public Shared Function GetSizeText(ByVal n As Long) As String
        If n < 1024 Then
            Return Math.Round(n, 3) & " Bytes"
        ElseIf n < 1048576 Then
            Return Math.Round(n / 1024, 3) & " KB"
        ElseIf n < 1073741824 Then
            Return Math.Round(n / 1048576, 3) & " MB"
        ElseIf n < 1099511627776 Then
            Return Math.Round(n / 1073741824, 3) & " GB"
        Else
            Return Math.Round(n / 1099511627776, 3) & " TB"
        End If
    End Function
    Public Shared Function Blend(color1 As Color, backColor As Color, amount As Double) As Color
        Dim r As Integer = ((color1.R * amount) + backColor.R * (1 - amount))
        Dim g As Integer = ((color1.G * amount) + backColor.G * (1 - amount))
        Dim b As Integer = ((color1.B * amount) + backColor.B * (1 - amount))
        Return Color.FromArgb(r, g, b)
    End Function
    Public Shared Function Blend(color1 As Color, backColor As Color) As Color
        Dim amount As Double = color1.A / 255
        Dim r As Integer = ((color1.R * amount) + backColor.R * (1 - amount))
        Dim g As Integer = ((color1.G * amount) + backColor.G * (1 - amount))
        Dim b As Integer = ((color1.B * amount) + backColor.B * (1 - amount))
        Return Color.FromArgb(r, g, b)
    End Function
    Public Shared Function Negative(color As Color) As Color
        Return Color.FromArgb(color.A, 255 - color.R, 255 - color.G, 255 - color.B)
    End Function
    Public Shared Function Rainbow(progress As Single) As Color
        Dim div As Single = (Math.Abs(progress Mod 1) * 6)
        Dim ascending As Integer = CInt((div Mod 1) * 255)
        Dim descending As Integer = 255 - ascending

        Select Case CInt(div)
            Case 0
                Return Color.FromArgb(255, 255, ascending, 0)
            Case 1
                Return Color.FromArgb(255, descending, 255, 0)
            Case 2
                Return Color.FromArgb(255, 0, 255, ascending)
            Case 3
                Return Color.FromArgb(255, 0, descending, 255)
            Case 4
                Return Color.FromArgb(255, ascending, 0, 255)
            Case Else
                ' case 5:
                Return Color.FromArgb(255, 255, 0, descending)
        End Select
    End Function

    Public Shared Sub CopyBytes(ByVal source As Byte(), ByVal sourceIndex As Integer, ByVal dest As Byte(), ByVal destIndex As Integer, ByVal length As Integer)
        For i As Integer = 0 To length - 1
            dest(destIndex + i) = source(sourceIndex + i)
        Next
    End Sub

    Public Shared Function GetTextHeight(ByVal hFont As IntPtr) As Single
        Dim hFontPrevious As IntPtr = GDI32.SelectObject(hDC, hFont)
        Dim tm As New GDI32.TEXTMETRIC
        GDI32.GetTextMetrics(hDC, tm)
        Return tm.tmHeight
    End Function
    Public Shared Function GetTextHeight(ByVal Font As Font) As Single
        Dim hFont As IntPtr = Font.ToHfont
        Dim hFontPrevious As IntPtr = GDI32.SelectObject(hDC, hFont)
        Dim tm As New GDI32.TEXTMETRIC
        GDI32.GetTextMetrics(hDC, tm)
        GDI32.SelectObject(hDC, hFontPrevious)
        GDI32.DeleteObject(hFont)
        Return tm.tmHeight
    End Function
    Public Shared Function GetRGB(color As Color) As Integer
        Dim b() As Byte = {color.R, color.G, color.B, 0}
        Return BitConverter.ToInt32(b, 0)
    End Function
    Public Shared Function GetColor(rgb As Integer) As Color
        Dim b() As Byte = BitConverter.GetBytes(rgb)
        Return Color.FromArgb(b(0), b(1), b(2))
    End Function
    Friend Shared Function GetABCWidths(ByVal hFont As IntPtr) As GDI32.AbcFloat()
        Dim hFontOld As IntPtr = GDI32.SelectObject(hDC, hFont)
        Dim rgAbcWidths(255) As GDI32.AbcFloat
        GDI32.GetCharABCWidthsFloat(hDC, 0, 255, rgAbcWidths)
        GDI32.SelectObject(hDC, hFontOld)
        Return rgAbcWidths
    End Function
    Friend Shared Function GetABCWidths(ByVal font As Font) As GDI32.AbcFloat()
        Dim hFont As IntPtr = font.ToHfont()
        Dim rgAbcWidths() As GDI32.AbcFloat = GetABCWidths(hFont)
        GDI32.DeleteObject(hFont)
        Return rgAbcWidths
    End Function
    Public Shared Function GetTextWidth(ByVal font As Font, ByVal s As String) As Single
        Dim abc As GDI32.AbcFloat() = GetABCWidths(font)
        Return GetTextWidth(abc, s)
    End Function
    Public Shared Function GetTextWidth(ByVal hFont As IntPtr, ByVal s As String) As Single
        Dim abc As GDI32.AbcFloat() = GetABCWidths(hFont)
        Return GetTextWidth(abc, s)
    End Function
    Friend Shared Function GetTextWidth(ByVal abcWidths As GDI32.AbcFloat(), ByVal s As String) As Single
        Dim byt As Byte = 0
        Dim w As Single = 0
        For Each i In s
            byt = Asc(i)
            w += abcWidths(byt).flA + abcWidths(byt).flB + abcWidths(byt).flC
        Next
        Return w
    End Function
    Friend Shared Function GetTextWidth(ByVal abcWidths As GDI32.AbcFloat(), ByVal textBytes As Byte()) As Single
        Dim w As Single = 0
        For Each i In textBytes
            w += abcWidths(i).flA + abcWidths(i).flB + abcWidths(i).flC
        Next
        Return w
    End Function

    Public Shared Sub FillRectangle(ByVal hDC As IntPtr, ByVal rect As Rectangle, ByVal color As Integer)
        Dim hBrush As IntPtr = GDI32.CreateSolidBrush(color)
        Dim hPen As IntPtr = GDI32.GetStockObject(GDI32.StockObjects.NULL_PEN)

        Dim hBrushPrevious As IntPtr = GDI32.SelectObject(hDC, hBrush)
        Dim hPenPrevious As IntPtr = GDI32.SelectObject(hDC, hPen)

        GDI32.Rectangle(hDC, rect.X, rect.Y, rect.Right + 1, rect.Bottom + 1)

        GDI32.SelectObject(hDC, hBrushPrevious)
        GDI32.SelectObject(hDC, hPenPrevious)

        GDI32.DeleteObject(hBrush)
        'GDI32.DeleteObject(hPen)
    End Sub
    Public Shared Sub DrawRectangle(ByVal hDC As IntPtr, ByVal rect As Rectangle, ByVal color As Integer)
        Dim hBrush As IntPtr = GDI32.GetStockObject(GDI32.StockObjects.NULL_BRUSH)
        Dim hPen As IntPtr = GDI32.CreatePen(GDI32.PenStyle.PS_SOLID, 1, color)

        Dim hBrushPrevious As IntPtr = GDI32.SelectObject(hDC, hBrush)
        Dim hPenPrevious As IntPtr = GDI32.SelectObject(hDC, hPen)

        GDI32.Rectangle(hDC, rect.X, rect.Y, rect.Right + 1, rect.Bottom + 1)

        GDI32.SelectObject(hDC, hBrushPrevious)
        GDI32.SelectObject(hDC, hPenPrevious)

        'GDI32.DeleteObject(hBrush)
        GDI32.DeleteObject(hPen)
    End Sub
    Public Shared Sub FillRectangle(ByVal hDC As IntPtr, ByVal rect As Rectangle, ByVal color As Integer, ByVal borderColor As Integer)
        Dim hBrush As IntPtr = GDI32.CreateSolidBrush(color)
        Dim hPen As IntPtr = GDI32.CreatePen(GDI32.PenStyle.PS_SOLID, 1, borderColor)

        Dim hBrushPrevious As IntPtr = GDI32.SelectObject(hDC, hBrush)
        Dim hPenPrevious As IntPtr = GDI32.SelectObject(hDC, hPen)

        GDI32.Rectangle(hDC, rect.X, rect.Y, rect.Right + 1, rect.Bottom + 1)

        GDI32.SelectObject(hDC, hBrushPrevious)
        GDI32.SelectObject(hDC, hPenPrevious)

        GDI32.DeleteObject(hBrush)
        GDI32.DeleteObject(hPen)
    End Sub
    Friend Shared Function Ceiling(ByVal n As Double) As Long
        Dim n2 As Long = Math.Ceiling(Math.Abs(n))
        If n < 0 Then
            n2 = -n2
        End If
        Return n2
    End Function

    Public Shared Function FlipCursor() As System.Windows.Forms.Cursor
        Dim cur As Cursor = Cursors.Arrow
        Dim ico As Icon = Icon.FromHandle(cur.Handle)
        Dim bmp As Bitmap = ico.ToBitmap
        Dim bmp2 As New Bitmap(bmp.Width + 16, bmp.Height + 29)
        Dim g As Graphics = Graphics.FromImage(bmp2)
        g.DrawImage(bmp, 22, 29)
        g.Dispose()
        bmp2.RotateFlip(RotateFlipType.Rotate180FlipY)
        ico = Icon.FromHandle(bmp2.GetHicon)
        Dim rcur As New Cursor(ico.Handle)
        bmp.Dispose()
        bmp2.Dispose()
        Return rcur
    End Function
End Class