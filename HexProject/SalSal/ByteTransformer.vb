Imports System.ComponentModel
Imports SalSal

Public Interface ITransformer
    ReadOnly Property Mode As TransformMode
    ReadOnly Property LengthPerData As Integer
    ReadOnly Property CharsPerData As Integer
    ReadOnly Property Sparator As Integer
    Function GetString(ByVal buffer As Byte(), ByVal index As Integer) As String
    Function GetBytes(ByVal text As String) As Byte()

End Interface

Friend Class AdvancedTransformInfo
    Public PerData As Integer
    Public CharsPerRow As Integer
    Public DataCount As Integer
    Public colFloor As Integer
    Public PartialCount As Integer
    Public Trans As ITransformer
    Public MaxAllCharsLength As Long

End Class
Public Class Transformers
    Public Shared Function Create(ByVal mode As TransformMode)
        Select Case mode
            Case TransformMode.CharView
                Return New CharViewTransformer()
            Case TransformMode.ByteView
                Return New ByteViewTransformer()
            Case TransformMode.BinaryView
                Return New BinaryViewTransformer()
            Case TransformMode.HexView
                Return New HexViewTransformer()
            Case TransformMode.Hex2View
                Return New Hex2ViewTransformer()
            Case TransformMode.Hex4View
                Return New Hex4ViewTransformer()
            Case TransformMode.Hex8View
                Return New Hex8ViewTransformer()
            Case TransformMode.Int16View
                Return New Int16ViewTransformer()
            Case TransformMode.Int32View
                Return New Int32ViewTransformer()
            Case TransformMode.UInt16View
                Return New UInt16ViewTransformer()
            Case TransformMode.UInt32View
                Return New UInt32ViewTransformer()
            Case TransformMode.Int64View
                Return New Int64ViewTransformer()
            Case TransformMode.UInt64View
                Return New UInt64ViewTransformer()
            Case TransformMode.FloatView
                Return New FloatViewTransformer()
            Case TransformMode.DoubleView
                Return New DoubleViewTransformer()
            Case TransformMode.UnicodeView
                Return New UnicodeViewTransformer()
            Case TransformMode.TextView
                Return New TextViewTransformer()
            Case TransformMode.TextWView
                Return New TextWViewTransformer()
        End Select

        Return Nothing
    End Function
    Friend Shared Function GetAdvancedTransformInfo(ByVal trans As ITransformer, ByVal col_count As Integer, ByVal afterLength As Integer) As AdvancedTransformInfo
        Dim ti As New AdvancedTransformInfo
        ti.PerData = trans.CharsPerData + trans.Sparator
        ti.DataCount = Math.Floor(col_count / trans.LengthPerData)
        ti.colFloor = ti.DataCount * trans.LengthPerData
        ti.CharsPerRow = (ti.DataCount * ti.PerData)
        Dim charsLessPerRow As Integer = (ti.DataCount * ti.PerData)
        ti.PartialCount = (col_count - ti.DataCount * trans.LengthPerData)
        ti.CharsPerRow += ti.PartialCount * 2
        ti.Trans = trans

        Dim maxCharsLength As Long = Math.Floor(afterLength / col_count) * ti.CharsPerRow
        Dim sisa As Integer = (afterLength Mod col_count)
        If sisa = 0 Then
            'maxCharsLength += ti.CharsPerRow
        ElseIf sisa Mod trans.LengthPerData = 0 Then
            maxCharsLength += sisa / trans.LengthPerData * ti.PerData
        Else
            maxCharsLength += Math.Floor(sisa / trans.LengthPerData) * ti.PerData

            sisa = sisa Mod trans.LengthPerData
            maxCharsLength += sisa * 2
        End If

        ti.MaxAllCharsLength = maxCharsLength

        Return ti
    End Function
    Public Class CharViewTransformer
        Implements ITransformer

        Friend cpd As Integer = 1
        Friend lpd As Integer = 1
        Friend spr As Integer = 0
        Friend tmd As TransformMode = TransformMode.CharView

        Public ReadOnly Property CharsPerData As Integer Implements ITransformer.CharsPerData
            Get
                Return cpd
            End Get
        End Property
        Public ReadOnly Property LengthPerData As Integer Implements ITransformer.LengthPerData
            Get
                Return lpd
            End Get
        End Property
        Public ReadOnly Property Mode As TransformMode Implements ITransformer.Mode
            Get
                Return tmd
            End Get
        End Property

        Public ReadOnly Property Sparator As Integer Implements ITransformer.Sparator
            Get
                Return spr
            End Get
        End Property

        Public Function GetBytes(text As String) As Byte() Implements ITransformer.GetBytes
            Return {Asc(text(0))}
        End Function

        Public Function GetString(buffer() As Byte, index As Integer) As String Implements ITransformer.GetString
            Return Chr(buffer(index))
        End Function
    End Class
    Public Class ByteViewTransformer
        Implements ITransformer

        Friend cpd As Integer = Math.Max(Byte.MinValue.ToString.Length, Byte.MaxValue.ToString.Length)
        Friend lpd As Integer = 1
        Friend spr As Integer = 1
        Friend tmd As TransformMode = TransformMode.ByteView

        Public ReadOnly Property CharsPerData As Integer Implements ITransformer.CharsPerData
            Get
                Return cpd
            End Get
        End Property
        Public ReadOnly Property LengthPerData As Integer Implements ITransformer.LengthPerData
            Get
                Return lpd
            End Get
        End Property
        Public ReadOnly Property Mode As TransformMode Implements ITransformer.Mode
            Get
                Return tmd
            End Get
        End Property

        Public ReadOnly Property Sparator As Integer Implements ITransformer.Sparator
            Get
                Return spr
            End Get
        End Property

        Public Function GetBytes(text As String) As Byte() Implements ITransformer.GetBytes
            Return {Int(text(0))}
        End Function

        Public Function GetString(buffer() As Byte, index As Integer) As String Implements ITransformer.GetString
            Return buffer(index).ToString
        End Function
    End Class
    Public Class BinaryViewTransformer
        Implements ITransformer

        Friend cpd As Integer = 8
        Friend lpd As Integer = 1
        Friend spr As Integer = 1
        Friend tmd As TransformMode = TransformMode.BinaryView

        Public ReadOnly Property CharsPerData As Integer Implements ITransformer.CharsPerData
            Get
                Return cpd
            End Get
        End Property
        Public ReadOnly Property LengthPerData As Integer Implements ITransformer.LengthPerData
            Get
                Return lpd
            End Get
        End Property
        Public ReadOnly Property Mode As TransformMode Implements ITransformer.Mode
            Get
                Return tmd
            End Get
        End Property

        Public ReadOnly Property Sparator As Integer Implements ITransformer.Sparator
            Get
                Return spr
            End Get
        End Property

        Public Function GetBytes(text As String) As Byte() Implements ITransformer.GetBytes
            Return {Helper.BinaryToByte(text)}
        End Function

        Public Function GetString(buffer() As Byte, index As Integer) As String Implements ITransformer.GetString
            Return Helper.Binary(buffer(index))
        End Function
    End Class
    Public Class HexViewTransformer
        Implements ITransformer

        Friend cpd As Integer = 2
        Friend lpd As Integer = 1
        Friend spr As Integer = 1
        Friend tmd As TransformMode = TransformMode.HexView

        Public ReadOnly Property CharsPerData As Integer Implements ITransformer.CharsPerData
            Get
                Return cpd
            End Get
        End Property
        Public ReadOnly Property LengthPerData As Integer Implements ITransformer.LengthPerData
            Get
                Return lpd
            End Get
        End Property
        Public ReadOnly Property Mode As TransformMode Implements ITransformer.Mode
            Get
                Return tmd
            End Get
        End Property
        Public ReadOnly Property Sparator As Integer Implements ITransformer.Sparator
            Get
                Return spr
            End Get
        End Property

        Public Function GetBytes(text As String) As Byte() Implements ITransformer.GetBytes
            text = text.Replace(" ", "")
            Return {"&H" & Mid(text, 1, 2)}
        End Function

        Public Function GetString(buffer() As Byte, index As Integer) As String Implements ITransformer.GetString
            Return Helper.Hex2(buffer(index))
        End Function
    End Class
    Public Class Hex2ViewTransformer
        Implements ITransformer

        Friend cpd As Integer = 4
        Friend lpd As Integer = 2
        Friend spr As Integer = 1
        Friend tmd As TransformMode = TransformMode.Hex2View

        Public ReadOnly Property CharsPerData As Integer Implements ITransformer.CharsPerData
            Get
                Return cpd
            End Get
        End Property
        Public ReadOnly Property LengthPerData As Integer Implements ITransformer.LengthPerData
            Get
                Return lpd
            End Get
        End Property
        Public ReadOnly Property Mode As TransformMode Implements ITransformer.Mode
            Get
                Return tmd
            End Get
        End Property
        Public ReadOnly Property Sparator As Integer Implements ITransformer.Sparator
            Get
                Return spr
            End Get
        End Property

        Public Function GetBytes(text As String) As Byte() Implements ITransformer.GetBytes
            Dim b(1) As Byte
            For i As Integer = 0 To 1
                b(i) = "&H" & Mid(text, i * 2 + 1, 2)
            Next
            Return b
        End Function

        Public Function GetString(buffer() As Byte, index As Integer) As String Implements ITransformer.GetString
            Return Helper.HexFlip(Helper.Hex4(BitConverter.ToInt16(buffer, index)))
        End Function
    End Class
    Public Class Hex4ViewTransformer
        Implements ITransformer

        Friend cpd As Integer = 8
        Friend lpd As Integer = 4
        Friend spr As Integer = 1
        Friend tmd As TransformMode = TransformMode.Hex4View

        Public ReadOnly Property CharsPerData As Integer Implements ITransformer.CharsPerData
            Get
                Return cpd
            End Get
        End Property
        Public ReadOnly Property LengthPerData As Integer Implements ITransformer.LengthPerData
            Get
                Return lpd
            End Get
        End Property
        Public ReadOnly Property Mode As TransformMode Implements ITransformer.Mode
            Get
                Return tmd
            End Get
        End Property
        Public ReadOnly Property Sparator As Integer Implements ITransformer.Sparator
            Get
                Return spr
            End Get
        End Property

        Public Function GetBytes(text As String) As Byte() Implements ITransformer.GetBytes
            Dim b(3) As Byte
            For i As Integer = 0 To 3
                b(i) = "&H" & Mid(text, i * 2 + 1, 2)
            Next
            Return b
        End Function

        Public Function GetString(buffer() As Byte, index As Integer) As String Implements ITransformer.GetString
            Return Helper.HexFlip(Helper.Hex8(BitConverter.ToInt32(buffer, index)))
        End Function
    End Class
    Public Class Hex8ViewTransformer
        Implements ITransformer

        Friend cpd As Integer = 16
        Friend lpd As Integer = 8
        Friend spr As Integer = 1
        Friend tmd As TransformMode = TransformMode.Hex8View

        Public ReadOnly Property CharsPerData As Integer Implements ITransformer.CharsPerData
            Get
                Return cpd
            End Get
        End Property
        Public ReadOnly Property LengthPerData As Integer Implements ITransformer.LengthPerData
            Get
                Return lpd
            End Get
        End Property
        Public ReadOnly Property Mode As TransformMode Implements ITransformer.Mode
            Get
                Return tmd
            End Get
        End Property
        Public ReadOnly Property Sparator As Integer Implements ITransformer.Sparator
            Get
                Return spr
            End Get
        End Property

        Public Function GetBytes(text As String) As Byte() Implements ITransformer.GetBytes
            Dim b(3) As Byte
            For i As Integer = 0 To 7
                b(i) = "&H" & Mid(text, i * 2 + 1, 2)
            Next
            Return b
        End Function

        Public Function GetString(buffer() As Byte, index As Integer) As String Implements ITransformer.GetString
            Return Helper.HexFlip(Helper.HexN(BitConverter.ToInt64(buffer, index), 16))
        End Function
    End Class
    Public Class Int16ViewTransformer
        Implements ITransformer

        Friend cpd As Integer = Math.Max(Short.MinValue.ToString.Length, Short.MaxValue.ToString.Length)
        Friend lpd As Integer = 2
        Friend spr As Integer = 1
        Friend tmd As TransformMode = TransformMode.Int16View

        Public ReadOnly Property CharsPerData As Integer Implements ITransformer.CharsPerData
            Get
                Return cpd
            End Get
        End Property
        Public ReadOnly Property LengthPerData As Integer Implements ITransformer.LengthPerData
            Get
                Return lpd
            End Get
        End Property
        Public ReadOnly Property Mode As TransformMode Implements ITransformer.Mode
            Get
                Return tmd
            End Get
        End Property
        Public ReadOnly Property Sparator As Integer Implements ITransformer.Sparator
            Get
                Return spr
            End Get
        End Property

        Public Function GetBytes(text As String) As Byte() Implements ITransformer.GetBytes
            Return BitConverter.GetBytes(CShort(text))
        End Function

        Public Function GetString(buffer() As Byte, index As Integer) As String Implements ITransformer.GetString
            Return BitConverter.ToInt16(buffer, index)
        End Function
    End Class
    Public Class Int32ViewTransformer
        Implements ITransformer

        Friend cpd As Integer = Math.Max(Integer.MinValue.ToString.Length, Integer.MaxValue.ToString.Length)
        Friend lpd As Integer = 4
        Friend spr As Integer = 1
        Friend tmd As TransformMode = TransformMode.Int32View

        Public ReadOnly Property CharsPerData As Integer Implements ITransformer.CharsPerData
            Get
                Return cpd
            End Get
        End Property
        Public ReadOnly Property LengthPerData As Integer Implements ITransformer.LengthPerData
            Get
                Return lpd
            End Get
        End Property
        Public ReadOnly Property Mode As TransformMode Implements ITransformer.Mode
            Get
                Return tmd
            End Get
        End Property
        Public ReadOnly Property Sparator As Integer Implements ITransformer.Sparator
            Get
                Return spr
            End Get
        End Property

        Public Function GetBytes(text As String) As Byte() Implements ITransformer.GetBytes
            Return BitConverter.GetBytes(CInt(text))
        End Function

        Public Function GetString(buffer() As Byte, index As Integer) As String Implements ITransformer.GetString
            Return BitConverter.ToInt32(buffer, index)
        End Function
    End Class
    Public Class UInt16ViewTransformer
        Implements ITransformer

        Friend cpd As Integer = Math.Max(UShort.MinValue.ToString.Length, UShort.MaxValue.ToString.Length)
        Friend lpd As Integer = 2
        Friend spr As Integer = 1
        Friend tmd As TransformMode = TransformMode.UInt16View

        Public ReadOnly Property CharsPerData As Integer Implements ITransformer.CharsPerData
            Get
                Return cpd
            End Get
        End Property
        Public ReadOnly Property LengthPerData As Integer Implements ITransformer.LengthPerData
            Get
                Return lpd
            End Get
        End Property
        Public ReadOnly Property Mode As TransformMode Implements ITransformer.Mode
            Get
                Return tmd
            End Get
        End Property
        Public ReadOnly Property Sparator As Integer Implements ITransformer.Sparator
            Get
                Return spr
            End Get
        End Property

        Public Function GetBytes(text As String) As Byte() Implements ITransformer.GetBytes
            Return BitConverter.GetBytes(CUShort(text))
        End Function

        Public Function GetString(buffer() As Byte, index As Integer) As String Implements ITransformer.GetString
            Return BitConverter.ToUInt16(buffer, index)
        End Function
    End Class
    Public Class UInt32ViewTransformer
        Implements ITransformer

        Friend cpd As Integer = Math.Max(UInteger.MinValue.ToString.Length, UInteger.MaxValue.ToString.Length)
        Friend lpd As Integer = 4
        Friend spr As Integer = 1
        Friend tmd As TransformMode = TransformMode.UInt32View

        Public ReadOnly Property CharsPerData As Integer Implements ITransformer.CharsPerData
            Get
                Return cpd
            End Get
        End Property
        Public ReadOnly Property LengthPerData As Integer Implements ITransformer.LengthPerData
            Get
                Return lpd
            End Get
        End Property
        Public ReadOnly Property Mode As TransformMode Implements ITransformer.Mode
            Get
                Return tmd
            End Get
        End Property
        Public ReadOnly Property Sparator As Integer Implements ITransformer.Sparator
            Get
                Return spr
            End Get
        End Property

        Public Function GetBytes(text As String) As Byte() Implements ITransformer.GetBytes
            Return BitConverter.GetBytes(CUInt(text))
        End Function

        Public Function GetString(buffer() As Byte, index As Integer) As String Implements ITransformer.GetString
            Return BitConverter.ToUInt32(buffer, index)
        End Function
    End Class
    Public Class Int64ViewTransformer
        Implements ITransformer

        Friend cpd As Integer = Math.Max(Long.MinValue.ToString.Length, Long.MaxValue.ToString.Length)
        Friend lpd As Integer = 8
        Friend spr As Integer = 1
        Friend tmd As TransformMode = TransformMode.Int64View

        Public ReadOnly Property CharsPerData As Integer Implements ITransformer.CharsPerData
            Get
                Return cpd
            End Get
        End Property
        Public ReadOnly Property LengthPerData As Integer Implements ITransformer.LengthPerData
            Get
                Return lpd
            End Get
        End Property
        Public ReadOnly Property Mode As TransformMode Implements ITransformer.Mode
            Get
                Return tmd
            End Get
        End Property
        Public ReadOnly Property Sparator As Integer Implements ITransformer.Sparator
            Get
                Return spr
            End Get
        End Property

        Public Function GetBytes(text As String) As Byte() Implements ITransformer.GetBytes
            Return BitConverter.GetBytes(CLng(text))
        End Function

        Public Function GetString(buffer() As Byte, index As Integer) As String Implements ITransformer.GetString
            Return BitConverter.ToInt64(buffer, index)
        End Function
    End Class
    Public Class UInt64ViewTransformer
        Implements ITransformer

        Friend cpd As Integer = Math.Max(ULong.MinValue.ToString.Length, ULong.MaxValue.ToString.Length)
        Friend lpd As Integer = 8
        Friend spr As Integer = 1
        Friend tmd As TransformMode = TransformMode.UInt64View

        Public ReadOnly Property CharsPerData As Integer Implements ITransformer.CharsPerData
            Get
                Return cpd
            End Get
        End Property
        Public ReadOnly Property LengthPerData As Integer Implements ITransformer.LengthPerData
            Get
                Return lpd
            End Get
        End Property
        Public ReadOnly Property Mode As TransformMode Implements ITransformer.Mode
            Get
                Return tmd
            End Get
        End Property
        Public ReadOnly Property Sparator As Integer Implements ITransformer.Sparator
            Get
                Return spr
            End Get
        End Property

        Public Function GetBytes(text As String) As Byte() Implements ITransformer.GetBytes
            Return BitConverter.GetBytes(CULng(text))
        End Function

        Public Function GetString(buffer() As Byte, index As Integer) As String Implements ITransformer.GetString
            Return BitConverter.ToUInt64(buffer, index)
        End Function
    End Class
    Public Class FloatViewTransformer
        Implements ITransformer

        Friend cpd As Integer = Math.Max(Single.MinValue.ToString.Length, Single.MaxValue.ToString.Length)
        Friend lpd As Integer = 4
        Friend spr As Integer = 1
        Friend tmd As TransformMode = TransformMode.FloatView

        Public ReadOnly Property CharsPerData As Integer Implements ITransformer.CharsPerData
            Get
                Return cpd
            End Get
        End Property
        Public ReadOnly Property LengthPerData As Integer Implements ITransformer.LengthPerData
            Get
                Return lpd
            End Get
        End Property
        Public ReadOnly Property Mode As TransformMode Implements ITransformer.Mode
            Get
                Return tmd
            End Get
        End Property
        Public ReadOnly Property Sparator As Integer Implements ITransformer.Sparator
            Get
                Return spr
            End Get
        End Property

        Public Function GetBytes(text As String) As Byte() Implements ITransformer.GetBytes
            Return BitConverter.GetBytes(CSng(text))
        End Function

        Public Function GetString(buffer() As Byte, index As Integer) As String Implements ITransformer.GetString
            Return BitConverter.ToSingle(buffer, index)
        End Function
    End Class
    Public Class DoubleViewTransformer
        Implements ITransformer

        Friend cpd As Integer = Math.Max(Double.MinValue.ToString.Length, Double.MaxValue.ToString.Length)
        Friend lpd As Integer = 8
        Friend spr As Integer = 1
        Friend tmd As TransformMode = TransformMode.DoubleView

        Public ReadOnly Property CharsPerData As Integer Implements ITransformer.CharsPerData
            Get
                Return cpd
            End Get
        End Property
        Public ReadOnly Property LengthPerData As Integer Implements ITransformer.LengthPerData
            Get
                Return lpd
            End Get
        End Property
        Public ReadOnly Property Mode As TransformMode Implements ITransformer.Mode
            Get
                Return tmd
            End Get
        End Property
        Public ReadOnly Property Sparator As Integer Implements ITransformer.Sparator
            Get
                Return spr
            End Get
        End Property

        Public Function GetBytes(text As String) As Byte() Implements ITransformer.GetBytes
            Return BitConverter.GetBytes(CDbl(text))
        End Function

        Public Function GetString(buffer() As Byte, index As Integer) As String Implements ITransformer.GetString
            Return BitConverter.ToDouble(buffer, index)
        End Function
    End Class
    Public Class UnicodeViewTransformer
        Implements ITransformer

        Friend cpd As Integer = 1
        Friend lpd As Integer = 2
        Friend spr As Integer = 0
        Friend tmd As TransformMode = TransformMode.UnicodeView

        Public ReadOnly Property CharsPerData As Integer Implements ITransformer.CharsPerData
            Get
                Return cpd
            End Get
        End Property
        Public ReadOnly Property LengthPerData As Integer Implements ITransformer.LengthPerData
            Get
                Return lpd
            End Get
        End Property
        Public ReadOnly Property Mode As TransformMode Implements ITransformer.Mode
            Get
                Return tmd
            End Get
        End Property

        Public ReadOnly Property Sparator As Integer Implements ITransformer.Sparator
            Get
                Return spr
            End Get
        End Property

        Public Function GetBytes(text As String) As Byte() Implements ITransformer.GetBytes
            Return {Asc(text(0))}
        End Function

        Public Function GetString(buffer() As Byte, index As Integer) As String Implements ITransformer.GetString
            Dim c As String = ChrW(BitConverter.ToUInt16(buffer, index))
            Return c
        End Function
    End Class
    Public Class TextViewTransformer
        Implements ITransformer

        Friend cpd As Integer = 8
        Friend lpd As Integer = 8
        Friend spr As Integer = 1
        Friend tmd As TransformMode = TransformMode.TextView

        Public Property Length As Integer
            Get
                Return lpd
            End Get
            Set(value As Integer)
                cpd = value
                lpd = value
            End Set
        End Property
        Public ReadOnly Property CharsPerData As Integer Implements ITransformer.CharsPerData
            Get
                Return cpd
            End Get
        End Property
        Public ReadOnly Property LengthPerData As Integer Implements ITransformer.LengthPerData
            Get
                Return lpd
            End Get
        End Property
        Public ReadOnly Property Mode As TransformMode Implements ITransformer.Mode
            Get
                Return tmd
            End Get
        End Property

        Public ReadOnly Property Sparator As Integer Implements ITransformer.Sparator
            Get
                Return spr
            End Get
        End Property

        Public Function GetBytes(text As String) As Byte() Implements ITransformer.GetBytes
            Dim b As Byte() = Helper.ansi.GetBytes(text, 0, cpd)
            If Not b.Length = cpd Then
                ReDim Preserve b(cpd - 1)
            End If
            Return b
        End Function

        Public Function GetString(buffer() As Byte, index As Integer) As String Implements ITransformer.GetString
            Return Helper.ansi.GetString(buffer, index, cpd)
        End Function
    End Class
    Public Class TextWViewTransformer
        Implements ITransformer

        Friend cpd As Integer = 4
        Friend lpd As Integer = 8
        Friend spr As Integer = 1
        Friend tmd As TransformMode = TransformMode.TextWView

        Public Property Length As Integer
            Get
                Return lpd
            End Get
            Set(value As Integer)
                cpd = value
                lpd = value * 2
            End Set
        End Property
        Public ReadOnly Property CharsPerData As Integer Implements ITransformer.CharsPerData
            Get
                Return cpd
            End Get
        End Property
        Public ReadOnly Property LengthPerData As Integer Implements ITransformer.LengthPerData
            Get
                Return lpd
            End Get
        End Property
        Public ReadOnly Property Mode As TransformMode Implements ITransformer.Mode
            Get
                Return tmd
            End Get
        End Property

        Public ReadOnly Property Sparator As Integer Implements ITransformer.Sparator
            Get
                Return spr
            End Get
        End Property

        Public Function GetBytes(text As String) As Byte() Implements ITransformer.GetBytes
            Dim b As Byte() = Helper.uni.GetBytes(text, 0, lpd)
            If Not b.Length = lpd Then
                ReDim Preserve b(lpd - 1)
            End If
            Return b
        End Function

        Public Function GetString(buffer() As Byte, index As Integer) As String Implements ITransformer.GetString
            Return Helper.uni.GetString(buffer, index, lpd)
        End Function
    End Class
End Class

<Browsable(True)>
Public Enum TransformMode As Integer
    HexView = 0
    Hex2View = 1
    Hex4View = 2
    Hex8View = 3

    ByteView = 4

    UInt16View = 5
    UInt32View = 6
    UInt64View = 7
    Int16View = 8
    Int32View = 9
    Int64View = 10

    CharView = 11
    UnicodeView = 12
    BinaryView = 13
    FloatView = 14
    DoubleView = 15

    TextView = 16
    TextWView = 17
End Enum
