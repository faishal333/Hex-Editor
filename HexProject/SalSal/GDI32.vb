Imports System.Drawing
Imports System.Runtime.InteropServices
Public Class GDI32
    <DllImport("gdi32.DLL", EntryPoint:="SelectObject", SetLastError:=True)>
    Public Shared Function SelectObject(hDC As IntPtr, hGDIObject As IntPtr) As IntPtr
    End Function

    <DllImport("gdi32.DLL", EntryPoint:="GetCharABCWidthsFloatA", SetLastError:=True)>
    Public Shared Function GetCharABCWidthsFloat(ByVal hDC As IntPtr, ByVal iFirst As Integer, ByVal iLast As Integer, <[In], Out> rgAbc As AbcFloat()) As Boolean
    End Function

    <DllImport("gdi32.DLL", EntryPoint:="GetCharABCWidthsFloatW", SetLastError:=True)>
    Public Shared Function GetCharABCWidthsFloatW(ByVal hDC As IntPtr, ByVal iFirst As Integer, ByVal iLast As Integer, <[In], Out> rgAbc As AbcFloat()) As Boolean
    End Function

    <DllImport("gdi32.DLL", EntryPoint:="GetCharWidthW", SetLastError:=True)>
    Public Shared Function GetCharWidthW(ByVal hDC As IntPtr, ByVal iFirst As Integer, ByVal iLast As Integer, <Out> lpBuffer As Integer) As Boolean
    End Function

    <DllImport("gdi32.DLL", EntryPoint:="GetTextExtentPoint32W", SetLastError:=True)>
    Public Shared Function GetTextExtentPoint32(ByVal hdc As IntPtr, ByVal lpString As String, ByVal c As Integer, ByRef lpSize As Size) As Boolean
    End Function

    <DllImport("gdi32.DLL", EntryPoint:="DeleteObject", SetLastError:=True)>
    Public Shared Function DeleteObject(ByVal hObject As IntPtr) As Boolean
    End Function

    <DllImport("gdi32.DLL", EntryPoint:="TextOutA", SetLastError:=True)>
    Public Shared Function TextOut(hDC As IntPtr, iXStart As Integer, iYStart As Integer, pwcStart As String, iLength As Integer) As Boolean
    End Function
    <DllImport("gdi32.DLL", EntryPoint:="TextOutW", SetLastError:=True)>
    Public Shared Function TextOutW(hDC As IntPtr, iXStart As Integer, iYStart As Integer, pwcStart As String, iLength As Integer) As Boolean
    End Function
    <DllImport("gdi32.DLL", EntryPoint:="TextOutW", SetLastError:=True)>
    Public Shared Function TextOutW2(hDC As IntPtr, iXStart As Integer, iYStart As Integer, ByRef pwcStart As Byte, iLength As Integer) As Boolean
    End Function

    <DllImport("gdi32.DLL", EntryPoint:="ExtTextOutA", SetLastError:=True)>
    Public Shared Function ExtTextOut(hDC As IntPtr, iXStart As Integer, iYStart As Integer, fuOptions As ETOOptions, ByRef lprc As Rectangle, pwcStart As String, iLength As Integer, lpDx As Integer) As Boolean
    End Function

    <DllImport("gdi32.DLL", EntryPoint:="SetTextColor", SetLastError:=True)>
    Public Shared Function SetTextColor(hDC As IntPtr, uiColor As Integer) As Integer
    End Function

    <DllImport("gdi32.DLL", EntryPoint:="SetBkColor", SetLastError:=True)>
    Public Shared Function SetBackColor(hDC As IntPtr, uiColor As Integer) As Integer
    End Function

    <DllImport("gdi32.DLL", EntryPoint:="SetBkMode", SetLastError:=True)>
    Public Shared Function SetBkMode(hDC As IntPtr, iBkMode As gdiBkMode) As Integer
    End Function

    <DllImport("Gdi32.dll", EntryPoint:="GetTextMetrics", CharSet:=CharSet.Auto)>
    Public Shared Function GetTextMetrics(hdc As IntPtr, ByRef lptm As TEXTMETRIC) As Integer
    End Function
    <DllImport("Gdi32.dll", EntryPoint:="RectVisible", CharSet:=CharSet.Auto)>
    Public Shared Function RectVisible(hdc As IntPtr, <[In]> ByRef rect As Rectangle) As Boolean
    End Function
    <DllImport("Gdi32.dll", EntryPoint:="PtVisible", CharSet:=CharSet.Auto)>
    Public Shared Function PtVisible(hdc As IntPtr, ByVal X As Integer, ByVal Y As Integer) As Boolean
    End Function
    <DllImport("gdi32.dll", EntryPoint:="BitBlt", CharSet:=CharSet.Auto)>
    Public Shared Function BitBlt(ByVal hdc As IntPtr, ByVal nXDest As Integer, ByVal nYDest As Integer, ByVal nWidth As Integer, ByVal nHeight As Integer, ByVal hdcSrc As IntPtr, ByVal nXSrc As Integer, ByVal nYSrc As Integer, ByVal dwRop As TernaryRasterOperations) As Boolean
    End Function
    <DllImport("gdi32.dll", EntryPoint:="CreateCompatibleDC", CharSet:=CharSet.Auto, SetLastError:=True)>
    Public Shared Function CreateCompatibleDC(ByVal hRefDC As IntPtr) As IntPtr
    End Function
    <DllImport("gdi32.dll", EntryPoint:="CreateCompatibleBitmap", CharSet:=CharSet.Auto)>
    Public Shared Function CreateCompatibleBitmap(hdc As IntPtr, nWidth As Integer, nHeight As Integer) As IntPtr
    End Function
    <DllImport("gdi32.dll", EntryPoint:="Rectangle", CharSet:=CharSet.Auto)>
    Public Shared Function Rectangle(hdc As IntPtr, nLeftRect As Integer, nTopRect As Integer, nRightRect As Integer, nBottomRect As Integer) As Boolean
    End Function
    <DllImport("gdi32.dll", EntryPoint:="CreateSolidBrush", CharSet:=CharSet.Auto)>
    Public Shared Function CreateSolidBrush(crColor As Integer) As IntPtr
    End Function
    <DllImport("gdi32.dll", EntryPoint:="CreatePatternBrush", CharSet:=CharSet.Auto)>
    Public Shared Function CreatePatternBrush(hbmp As IntPtr) As IntPtr
    End Function
    <DllImport("gdi32.dll", EntryPoint:="CreatePen", CharSet:=CharSet.Auto)>
    Public Shared Function CreatePen(fnPenStyle As PenStyle, nWidth As Integer, crColor As Integer) As IntPtr
    End Function
    <DllImport("gdi32.dll", EntryPoint:="SaveDC", CharSet:=CharSet.Auto)>
    Public Shared Function SaveDC(hdc As IntPtr) As Integer
    End Function
    <DllImport("gdi32.dll", EntryPoint:="RestoreDC", CharSet:=CharSet.Auto)>
    Public Shared Function RestoreDC(hdc As IntPtr, nSavedDC As Integer) As IntPtr
    End Function
    <DllImport("gdi32.dll", EntryPoint:="GetPixel", CharSet:=CharSet.Auto)>
    Public Shared Function GetPixel(hdc As IntPtr, nXPos As Integer, nYPos As Integer) As Integer
    End Function
    <DllImport("gdi32.dll", EntryPoint:="SetPixel", CharSet:=CharSet.Auto)>
    Public Shared Function SetPixel(hdc As IntPtr, nXPos As Integer, nYPos As Integer, crColor As Integer) As Integer
    End Function
    <DllImport("gdi32.dll", EntryPoint:="SelectClipRgn", CharSet:=CharSet.Auto)>
    Public Shared Function SelectClipRgn(hdc As IntPtr, hrgn As IntPtr) As Integer
    End Function

    <DllImport("gdi32.dll", EntryPoint:="CreateRectRgn", CharSet:=CharSet.Auto)>
    Public Shared Function CreateRectRgn(nLeftRect As Integer, nTopRect As Integer, nRightRect As Integer, nBottomRect As Integer) As IntPtr
    End Function
    <DllImport("gdi32.dll")>
    Public Shared Function GetStockObject(fnObject As StockObjects) As IntPtr
    End Function
    <DllImport("gdi32.dll")>
    Public Shared Function CreatePalette(ByRef lpLogPalette As LOGPALETTE) As IntPtr
    End Function
    <DllImport("gdi32.dll")>
    Public Shared Function SelectPalette(hdc As IntPtr, hpal As IntPtr, bForceBackground As Boolean) As IntPtr
    End Function
    <DllImport("gdi32.dll")>
    Public Shared Function RealizePalette(hdc As IntPtr) As UInteger
    End Function
    <DllImport("gdi32.dll")>
    Public Shared Function SetWorldTransform(ByVal hdc As IntPtr, <[In]> ByRef lpXform As XFORM) As Boolean
    End Function
    <StructLayout(LayoutKind.Sequential, CharSet:=CharSet.Auto)>
    Public Structure XFORM
        Public eM11 As Single
        Public eM12 As Single
        Public eM21 As Single
        Public eM22 As Single
        Public eDx As Single
        Public eDy As Single
    End Structure

    Public Enum TernaryRasterOperations As UInteger
        ''' <summary>dest = source</summary>
        SRCCOPY = &HCC0020
        ''' <summary>dest = source OR dest</summary>
        SRCPAINT = &HEE0086
        ''' <summary>dest = source AND dest</summary>
        SRCAND = &H8800C6
        ''' <summary>dest = source XOR dest</summary>
        SRCINVERT = &H660046
        ''' <summary>dest = source AND (NOT dest)</summary>
        SRCERASE = &H440328
        ''' <summary>dest = (NOT source)</summary>
        NOTSRCCOPY = &H330008
        ''' <summary>dest = (NOT src) AND (NOT dest)</summary>
        NOTSRCERASE = &H1100A6
        ''' <summary>dest = (source AND pattern)</summary>
        MERGECOPY = &HC000CA
        ''' <summary>dest = (NOT source) OR dest</summary>
        MERGEPAINT = &HBB0226
        ''' <summary>dest = pattern</summary>
        PATCOPY = &HF00021
        ''' <summary>dest = DPSnoo</summary>
        PATPAINT = &HFB0A09
        ''' <summary>dest = pattern XOR dest</summary>
        PATINVERT = &H5A0049
        ''' <summary>dest = (NOT dest)</summary>
        DSTINVERT = &H550009
        ''' <summary>dest = BLACK</summary>
        BLACKNESS = &H42
        ''' <summary>dest = WHITE</summary>
        WHITENESS = &HFF0062
        ''' <summary>
        ''' Capture window as seen on screen.  This includes layered windows
        ''' such as WPF windows with AllowsTransparency="true"
        ''' </summary>
        CAPTUREBLT = &H40000000
    End Enum
    <StructLayout(LayoutKind.Sequential, CharSet:=CharSet.Auto)>
    Public Structure AbcFloat
        Public flA As Single
        Public flB As Single
        Public flC As Single
    End Structure
    Public Enum gdiBkMode As Integer
        TRANSPARENT = 1
        OPAQUE = 2
    End Enum
    <StructLayout(LayoutKind.Sequential, CharSet:=CharSet.Auto)>
    Public Structure TEXTMETRIC
        Public tmHeight As Integer
        Public tmAscent As Integer
        Public tmDescent As Integer
        Public tmInternalLeading As Integer
        Public tmExternalLeading As Integer
        Public tmAveCharWidth As Integer
        Public tmMaxCharWidth As Integer
        Public tmWeight As Integer
        Public tmOverhang As Integer
        Public tmDigitizedAspectX As Integer
        Public tmDigitizedAspectY As Integer
        Public tmFirstChar As Char
        Public tmLastChar As Char
        Public tmDefaultChar As Char
        Public tmBreakChar As Char
        Public tmItalic As Byte
        Public tmUnderlined As Byte
        Public tmStruckOut As Byte
        Public tmPitchAndFamily As Byte
        Public tmCharSet As Byte
    End Structure
    Public Enum PenStyle As Integer
        PS_SOLID = 0
        PS_DASH = 1
        PS_DOT = 2
        PS_DASHDOT = 3
        PS_DASHDOTDOT = 4
        PS_NULL = 5
        PS_INSIDEFRAME = 6
    End Enum
    Public Enum StockObjects As Integer
        WHITE_BRUSH = 0
        LTGRAY_BRUSH = 1
        GRAY_BRUSH = 2
        DKGRAY_BRUSH = 3
        BLACK_BRUSH = 4
        NULL_BRUSH = 5
        HOLLOW_BRUSH = NULL_BRUSH
        WHITE_PEN = 6
        BLACK_PEN = 7
        NULL_PEN = 8
        OEM_FIXED_FONT = 10
        ANSI_FIXED_FONT = 11
        ANSI_VAR_FONT = 12
        SYSTEM_FONT = 13
        DEVICE_DEFAULT_FONT = 14
        DEFAULT_PALETTE = 15
        SYSTEM_FIXED_FONT = 16
        DEFAULT_GUI_FONT = 17
        DC_BRUSH = 18
        DC_PEN = 19
    End Enum
    Public Enum ETOOptions As Integer
        ETO_CLIPPED = &H4
        ETO_GLYPH_INDEX = &H10
        ETO_IGNORELANGUAGE = &H1000
        ETO_NUMERICSLATIN = &H800
        ETO_NUMERICSLOCAL = &H400
        ETO_OPAQUE = &H2
        ETO_PDY = &H2000
        ETO_RTLREADING = &H800
    End Enum
    <StructLayout(LayoutKind.Sequential, Pack:=4)>
    Public Structure PALETTEENTRY
        Public peRed As Byte
        Public peGreen As Byte
        Public peBlue As Byte
        Public peFlags As Byte
    End Structure

    <StructLayout(LayoutKind.Sequential, Pack:=4)>
    Public Structure LOGPALETTE
        Public palVersion As Short
        Public palNumEntries As Short
        <MarshalAs(UnmanagedType.ByValArray, SizeConst:=200)>
        Public palette As PALETTEENTRY()
    End Structure
End Class