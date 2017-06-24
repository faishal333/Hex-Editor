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
        End Select

        Return Nothing
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

        Friend Shared replaced_chars As Char() = {Chr(0), Chr(1), Chr(2), Chr(3), Chr(4), Chr(5), Chr(6), Chr(7), Chr(8), Chr(9), Chr(&HA), Chr(&HB), Chr(&HC), Chr(&HD), Chr(&HE), Chr(&HF),
                                         Chr(&H10), Chr(&H11), Chr(&H12), Chr(&H13), Chr(&H14), Chr(&H15), Chr(&H16), Chr(&H17), Chr(&H18), Chr(&H19), Chr(&H10), Chr(&H1A), Chr(&H1B), Chr(&H1C), Chr(&H1D), Chr(&H1E), Chr(&H1F),
                                        Chr(&H20), Chr(&H7F), Chr(&H81), Chr(&H8D), Chr(&H8F), Chr(&H90), Chr(&H98), Chr(&H9D), Chr(&HAD)}
        Friend Shared Function CharReplace(ByVal tx As String) As String
            For Each i In replaced_chars
                tx = tx.Replace(i, ".")
            Next
            Return tx
        End Function

        Public Function GetString(buffer() As Byte, index As Integer) As String Implements ITransformer.GetString
            Return CharReplace(Chr(buffer(index)))
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

        Friend Shared replaced_chars As Char() = {Chr(0), Chr(1), Chr(2), Chr(3), Chr(4), Chr(5), Chr(6), Chr(7), Chr(8), Chr(9), Chr(&HA), Chr(&HB), Chr(&HC), Chr(&HD), Chr(&HE), Chr(&HF),
                                         Chr(&H10), Chr(&H11), Chr(&H12), Chr(&H13), Chr(&H14), Chr(&H15), Chr(&H16), Chr(&H17), Chr(&H18), Chr(&H19), Chr(&H10), Chr(&H1A), Chr(&H1B), Chr(&H1C), Chr(&H1D), Chr(&H1E), Chr(&H1F),
                                        Chr(&H20), Chr(&H7F), Chr(&H81), Chr(&H8D), Chr(&H8F), Chr(&H90), Chr(&H98), Chr(&H9D), Chr(&HAD)}
        Friend Shared Function CharReplace(ByVal tx As String) As String
            For Each i In replaced_chars
                tx = tx.Replace(i, ".")
            Next
            Return tx
        End Function

        Public Function GetString(buffer() As Byte, index As Integer) As String Implements ITransformer.GetString
            Return CharReplace(ChrW(buffer(index)))
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
End Enum
