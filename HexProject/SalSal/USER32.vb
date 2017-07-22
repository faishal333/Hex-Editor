Imports System.Drawing
Imports System.Runtime.InteropServices

Public Class USER32
    <DllImport("user32.dll", SetLastError:=True, CharSet:=CharSet.Auto)>
    Public Shared Function ShowScrollBar(ByVal hWnd As System.IntPtr, ByVal wBar As Integer, ByVal bShow As Boolean) As Boolean
    End Function

    <DllImport("user32.dll", SetLastError:=True, CharSet:=CharSet.Auto)>
    Public Shared Function SetForegroundWindow(ByVal hWnd As IntPtr) As Boolean
    End Function

    <DllImport("user32.dll", SetLastError:=True, CharSet:=CharSet.Auto)>
    Public Shared Function CreatePopupMenu() As IntPtr
    End Function
    <DllImport("user32.dll", SetLastError:=True, CharSet:=CharSet.Auto)>
    Public Shared Function CreateMenu() As IntPtr
    End Function
    <DllImport("user32.dll", CharSet:=CharSet.Auto)>
    Public Shared Function AppendMenu(ByVal hMenu As IntPtr, ByVal uFlags As MenuFlags, ByVal uIDNewItem As Integer, ByVal lpNewItem As String) As Integer
    End Function
    <DllImport("User32.Dll", EntryPoint:="TrackPopupMenuEx")>
    Public Shared Function TrackPopupMenuEx(ByVal hMenu As IntPtr, ByVal fuFlags As UInteger, ByVal x As Integer, ByVal y As Integer, ByVal hwnd As IntPtr, ByVal lptpm As IntPtr) As Integer
    End Function
    <DllImport("User32.Dll", EntryPoint:="InsertMenuA")>
    Public Shared Function InsertMenu(ByVal hMenu As IntPtr, ByVal uPosition As Integer, ByVal uFlags As Integer, ByVal uIDNewItem As IntPtr, ByVal lpNewItem As String) As Integer
    End Function
    <DllImport("User32.Dll", EntryPoint:="InsertMenuItemA")>
    Public Shared Function InsertMenuItem(ByVal hMenu As IntPtr, ByVal uItem As Integer, ByVal fByPosition As Boolean, ByRef lpmii As MENUITEMINFO) As Boolean
    End Function

    <DllImport("user32.dll", CharSet:=CharSet.Auto, SetLastError:=False)>
    Public Shared Function GetSubMenu(ByVal hMenu As IntPtr, ByVal nPos As Integer) As IntPtr
    End Function
    Public Enum MenuFlags As Integer
        MF_INSERT = &H0
        MF_CHANGE = &H80
        MF_APPEND = &H100
        MF_DELETE = &H200
        MF_REMOVE = &H1000
        MF_BYCOMMAND = &H0
        MF_BYPOSITION = &H400
        MF_SEPARATOR = &H800
        MF_ENABLED = &H0
        MF_GRAYED = &H1
        MF_DISABLED = &H2
        MF_UNCHECKED = &H0
        MF_CHECKED = &H8
        MF_USECHECKBITMAPS = &H200
        MF_STRING = &H0
        MF_BITMAP = &H4
        MF_OWNERDRAW = &H100
        MF_POPUP = &H10
        MF_MENUBARBREAK = &H20
        MF_MENUBREAK = &H40
        MF_UNHILITE = &H0
        MF_HILITE = &H80
        MF_DEFAULT = &H1000
        MF_SYSMENU = &H2000
        MF_HELP = &H4000
        MF_RIGHTJUSTIFY = &H4000
        MF_MOUSESELECT = &H8000
    End Enum
    Public Enum TrackPopupFlag As Integer
        TPM_CENTERALIGN = &H4
        TPM_LEFTALIGN = &H0
        TPM_RIGHTALIGN = &H8
        TPM_BOTTOMALIGN = &H20
        TPM_TOPALIGN = &H0
        TPM_VCENTERALIGN = &H10
        TPM_NONOTIFY = &H80
        TPM_RETURNCMD = &H100
        TPM_LEFTBUTTON = &H0
        TPM_RIGHTBUTTON = &H2
        TPM_HORNEGANIMATION = &H800
        TPM_HORPOSANIMATION = &H400
        TPM_NOANIMATION = &H4000
        TPM_VERNEGANIMATION = &H2000
        TPM_VERPOSANIMATION = &H1000
    End Enum

    <DllImport("kernel32.dll")>
    Public Shared Function GetLastError() As Integer
    End Function
    <DllImport("user32.dll", SetLastError:=True, ThrowOnUnmappableChar:=True, CharSet:=CharSet.Auto)>
    Public Shared Function SetScrollInfo(hWnd As IntPtr, ByVal nBar As SBOrientation, ByRef lpsi As SCROLLINFO, ByVal bRepaint As Boolean) As Boolean
    End Function
    <DllImport("user32.dll", SetLastError:=True)>
    Public Shared Function GetScrollInfo(ByVal hWnd As IntPtr, ByVal fnBar As SBOrientation, ByRef lpsi As SCROLLINFO) As Boolean
    End Function
    <DllImport("user32.dll", SetLastError:=True)>
    Public Shared Function GetCursor() As IntPtr
    End Function
    <DllImport("user32.dll", SetLastError:=True)>
    Public Shared Function SetCursor(ByVal hCursor As IntPtr) As IntPtr
    End Function
    <DllImport("user32.dll", SetLastError:=True)>
    Public Shared Function GetCursorInfo(ByRef pci As CURSORINFO) As Boolean
    End Function
    <DllImport("user32.dll")>
    Shared Function CreateCursor(ByVal hInst As IntPtr, ByVal xHotSpot As Integer, ByVal yHotSpot As Integer, ByVal nWidth As Integer, ByVal nHeight As Integer, ByVal pvANDPlane() As Byte, ByVal pvXORPlane() As Byte) As IntPtr
    End Function
    <DllImport("user32.dll", SetLastError:=True)>
    Public Shared Function DestroyCursor(ByVal hCursor As IntPtr) As Boolean
    End Function
    <System.Runtime.InteropServices.DllImportAttribute("user32.dll", EntryPoint:="GetIconInfo")>
    Public Shared Function GetIconInfo(ByVal hIcon As System.IntPtr, ByRef piconinfo As ICONINFO) As Boolean
    End Function
    <DllImport("user32.dll", SetLastError:=True)>
    Public Shared Function DrawIcon(ByVal hDC As IntPtr, ByVal X As Integer, ByVal Y As Integer, ByVal hIcon As IntPtr) As Boolean
    End Function
    Public Declare Auto Function SetLayeredWindowAttributes Lib "User32.Dll" _
    (ByVal hWnd As IntPtr, ByVal crKey As Integer, ByVal Alpha As Byte, ByVal dwFlags As Integer) As Boolean
    <StructLayout(LayoutKind.Sequential, CharSet:=CharSet.Auto)>
    Public Enum ScrollBarDirection As Integer
        SB_HORZ = 0
        SB_VERT = 1
        SB_CTL = 2
        SB_BOTH = 3
    End Enum
    Public Enum SBOrientation As Integer
        SB_HORZ = &H0
        SB_VERT = &H1
        SB_CTL = &H2
        SB_BOTH = &H3
    End Enum
    <Serializable, StructLayout(LayoutKind.Sequential)>
    Public Structure SCROLLINFO
        Public cbSize As UInteger
        <MarshalAs(UnmanagedType.U4)> Public fMask As ScrollInfoMask
        Public nMin As Integer
        Public nMax As Integer
        Public nPage As UInteger
        Public nPos As Integer
        Public nTrackPos As Integer
    End Structure
    <StructLayout(LayoutKind.Sequential)>
    Public Structure CURSORINFO
        Public cbSize As Integer
        Public flags As Integer
        Public hCursor As Integer
        Public ptScreenPos As Drawing.Point
    End Structure
    Public Enum ScrollInfoMask As UInteger
        SIF_RANGE = &H1
        SIF_PAGE = &H2
        SIF_POS = &H4
        SIF_DISABLENOSCROLL = &H8
        SIF_TRACKPOS = &H10
        SIF_ALL = (SIF_RANGE Or SIF_PAGE Or SIF_POS Or SIF_TRACKPOS)
    End Enum
    <StructLayout(LayoutKind.Sequential, CharSet:=CharSet.Auto)>
    Public Enum WindowStyles As Long

        Todo1 = 2
        Todo2 = 2048
        Todo3 = 32768

        WS_OVERLAPPED = 0
        WS_POPUP = 2147483648
        WS_CHILD = 1073741824
        WS_MINIMIZE = 536870912
        WS_VISIBLE = 268435456
        WS_DISABLED = 134217728
        WS_CLIPSIBLINGS = 67108864
        WS_CLIPCHILDREN = 33554432
        WS_MAXIMIZE = 16777216
        WS_BORDER = 8388608
        WS_DLGFRAME = 4194304
        WS_VSCROLL = 2097152
        WS_HSCROLL = 1048576
        WS_SYSMENU = 524288
        WS_THICKFRAME = 262144
        WS_GROUP = 131072
        WS_TABSTOP = 65536

        WS_MINIMIZEBOX = 131072
        WS_MAXIMIZEBOX = 65536

        WS_CAPTION = WS_BORDER Or WS_DLGFRAME
        WS_TILED = WS_OVERLAPPED
        WS_ICONIC = WS_MINIMIZE
        WS_SIZEBOX = WS_THICKFRAME
        WS_TILEDWINDOW = WS_OVERLAPPEDWINDOW

        WS_OVERLAPPEDWINDOW = WS_OVERLAPPED Or WS_CAPTION Or WS_SYSMENU Or
                  WS_THICKFRAME Or WS_MINIMIZEBOX Or WS_MAXIMIZEBOX
        WS_POPUPWINDOW = WS_POPUP Or WS_BORDER Or WS_SYSMENU
        WS_CHILDWINDOW = WS_CHILD

        WS_EX_DLGMODALFRAME = 1
        WS_EX_NOPARENTNOTIFY = 4
        WS_EX_TOPMOST = 8
        WS_EX_ACCEPTFILES = 16
        WS_EX_TRANSPARENT = 32

        '#If (WINVER >= 400) Then
        WS_EX_MDICHILD = 64
        WS_EX_TOOLWINDOW = 128
        WS_EX_WINDOWEDGE = 256
        WS_EX_CLIENTEDGE = 512
        WS_EX_CONTEXTHELP = 1024

        WS_EX_RIGHT = 4096
        WS_EX_LEFT = 0
        WS_EX_RTLREADING = 8192
        WS_EX_LTRREADING = 0
        WS_EX_LEFTSCROLLBAR = 16384
        WS_EX_RIGHTSCROLLBAR = 0

        WS_EX_CONTROLPARENT = 65536
        WS_EX_STATICEDGE = 131072
        WS_EX_APPWINDOW = 262144

        WS_EX_OVERLAPPEDWINDOW = WS_EX_WINDOWEDGE Or WS_EX_CLIENTEDGE
        WS_EX_PALETTEWINDOW = WS_EX_WINDOWEDGE Or WS_EX_TOOLWINDOW Or WS_EX_TOPMOST
        '#End If

        '#If (WIN32WINNT >= 500) Then
        WS_EX_LAYERED = 524288
        '#End If

        '#If (WINVER >= 500) Then
        WS_EX_NOINHERITLAYOUT = 1048576 ' Disable inheritence of mirroring by children
        WS_EX_LAYOUTRTL = 4194304 ' Right to left mirroring
        '#End If

        '#If (WIN32WINNT >= 500) Then
        WS_EX_COMPOSITED = 33554432
        WS_EX_NOACTIVATE = 67108864
        '#End If

    End Enum
    <StructLayout(LayoutKind.Sequential)>
    Structure ICONINFO
        Public fIcon As Boolean
        ' Specifies whether this structure defines an icon or a cursor. A value of TRUE specifies
        ' an icon; FALSE specifies a cursor.
        Public xHotspot As Int32
        ' Specifies the x-coordinate of a cursor's hot spot. If this structure defines an icon, the hot
        ' spot is always in the center of the icon, and this member is ignored.
        Public yHotspot As Int32
        ' Specifies the y-coordinate of the cursor's hot spot. If this structure defines an icon, the hot
        ' spot is always in the center of the icon, and this member is ignored.
        Public hbmMask As IntPtr
        ' (HBITMAP) Specifies the icon bitmask bitmap. If this structure defines a black and white icon,
        ' this bitmask is formatted so that the upper half is the icon AND bitmask and the lower half is
        ' the icon XOR bitmask. Under this condition, the height should be an even multiple of two. If
        ' this structure defines a color icon, this mask only defines the AND bitmask of the icon.
        Public hbmColor As IntPtr
        ' (HBITMAP) Handle to the icon color bitmap. This member can be optional if this
        ' structure defines a black and white icon. The AND bitmask of hbmMask is applied with the SRCAND
        ' flag to the destination; subsequently, the color bitmap is applied (using XOR) to the
        ' destination by using the SRCINVERT flag.
    End Structure

    <StructLayout(LayoutKind.Sequential)>
    Public Structure MENUITEMINFO
        Public cbSize As Integer
        Public fMask As Integer
        Public fType As Integer
        Public fState As Integer
        Public wID As Integer
        Public hSubMenu As IntPtr
        Public hbmpChecked As IntPtr
        Public hbmpUnchecked As IntPtr
        Public dwItemData As IntPtr
        Public dwTypeData As String
        Public cch As Integer
        Public hbmpItem As IntPtr
    End Structure

    Public Enum MenuMasks As Integer
        MIIM_STATE = &H1
        MIIM_ID = &H2
        MIIM_SUBMENU = &H4
        MIIM_CHECKMARKS = &H8
        MIIM_TYPE = &H10
        MIIM_DATA = &H20
        MIIM_STRING = &H40
        MIIM_BITMAP = &H80
        MIIM_FTYPE = &H100
        MFT_STRING = MenuFlags.MF_STRING
        MFT_BITMAP = MenuFlags.MF_BITMAP
        MFT_MENUBARBREAK = MenuFlags.MF_MENUBARBREAK
        MFT_MENUBREAK = MenuFlags.MF_MENUBREAK
        MFT_OWNERDRAW = MenuFlags.MF_OWNERDRAW
        MFT_RADIOCHECK = &H200
        MFT_SEPARATOR = MenuFlags.MF_SEPARATOR
        MFT_RIGHTORDER = &H2000
        MFT_RIGHTJUSTIFY = MenuFlags.MF_RIGHTJUSTIFY

    End Enum
    Public Enum MenuStates As Integer

        MFS_CHECKED = &H8

        MFS_DEFAULT = &H1000

        MFS_DISABLED = &H3

        MFS_ENABLED = &H0

        MFS_GRAYED = &H3

        MFS_HILITE = &H80

        MFS_UNCHECKED = &H0

        MFS_UNHILITE = &H0

    End Enum
End Class
