Imports System.Security.Permissions
Imports System.Windows.Forms

Friend Class GotoDialog
    Inherits System.Windows.Forms.CommonDialog
    Friend frm As GotoDialogForm

    Friend Event Apply As EventHandler

    Public Sub New()
        frm = New GotoDialogForm
    End Sub
    Public Overrides Sub Reset()
        ValueMode = ValueModes.Offset
        HexMode = True
        Value = 0
    End Sub

    Protected Overrides Function RunDialog(hwndOwner As IntPtr) As Boolean
        If frm.ShowDialog(New IwndParent(hwndOwner)) = DialogResult.OK Then
            Return True
        Else
            Return False
        End If
    End Function

    Protected Overridable ReadOnly Property Instance As IntPtr
        <SecurityPermission(SecurityAction.LinkDemand, Flags:=SecurityPermissionFlag.UnmanagedCode), SecurityPermission(SecurityAction.InheritanceDemand, Flags:=SecurityPermissionFlag.UnmanagedCode)>
        Get
            Return frm.Handle
        End Get
    End Property
    Public Property Collumn As Integer
        Get
            Return frm.col_count
        End Get
        Set(value As Integer)
            If value < 1 Then value = 1
            frm.col_count = value
        End Set
    End Property
    Public Property HexMode As Boolean
        Get
            Return frm.CheckBox1.Checked
        End Get
        Set(value As Boolean)
            If Not frm.CheckBox1.Checked = value Then
                frm.CheckBox1.Checked = value
                If value Then
                    Dim tx As String = frm.TextBox1.Text
                    Dim v As Long = 0
                    If IsNumeric(tx) Then
                        v = tx
                    End If
                    If v < 0 Then v = 0
                    Me.Value = v
                Else
                    Dim tx As String = "&H" & frm.TextBox1.Text
                    If CStr(tx.Last).ToLower = "h" Then
                        tx = tx.Remove(tx.Length - 1)
                    End If
                    Dim v As Long = 0
                    If IsNumeric(tx) Then
                        v = tx
                    End If
                    If v < 0 Then v = 0
                    Me.Value = v
                End If
            End If

        End Set
    End Property
    Public Property ValueMode As ValueModes
        Get
            If frm.RadioButton1.Checked Then
                Return ValueModes.Offset
            Else
                Return ValueModes.Line
            End If
        End Get
        Set(value As ValueModes)
            If value = ValueModes.Offset Then
                frm.RadioButton1.Checked = True
            Else
                frm.RadioButton2.Checked = True
            End If
        End Set
    End Property
    Public Property Value As Long
        Get
            Dim tx As String = ""
            If HexMode Then
                tx = "&H" & frm.TextBox1.Text
                If CStr(tx.Last).ToLower = "h" Then
                    tx = tx.Remove(tx.Length - 1)
                End If
            Else
                tx = frm.TextBox1.Text
            End If

            Dim v As Long = 0
            If IsNumeric(tx) Then
                v = tx
            End If
            If v < 0 Then v = 0
            Return v
        End Get
        Set(value As Long)
            If HexMode Then
                frm.TextBox1.Text = Hex(value) & "h"
            Else
                frm.TextBox1.Text = value
            End If
        End Set
    End Property
End Class
Friend Class IwndParent
    Implements System.Windows.Forms.IWin32Window
    Friend h As IntPtr
    Public Sub New(ByVal handle As IntPtr)
        h = handle
    End Sub
    Public ReadOnly Property Handle As IntPtr Implements IWin32Window.Handle
        Get
            Return h
        End Get
    End Property
End Class
Friend Class GotoDialogForm
    Inherits System.Windows.Forms.Form
    Public Sub New()
        InitializeComponent()
    End Sub
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer
    Friend col_count As Integer = 1

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    Private Sub InitializeComponent()
        Me.TableLayoutPanel1 = New System.Windows.Forms.TableLayoutPanel()
        Me.OK_Button = New System.Windows.Forms.Button()
        Me.Cancel_Button = New System.Windows.Forms.Button()
        Me.TableLayoutPanel2 = New System.Windows.Forms.TableLayoutPanel()
        Me.TextBox1 = New System.Windows.Forms.TextBox()
        Me.RadioButton1 = New System.Windows.Forms.RadioButton()
        Me.FlowLayoutPanel1 = New System.Windows.Forms.FlowLayoutPanel()
        Me.RadioButton2 = New System.Windows.Forms.RadioButton()
        Me.CheckBox1 = New System.Windows.Forms.CheckBox()
        Me.TableLayoutPanel3 = New System.Windows.Forms.TableLayoutPanel()
        Me.TableLayoutPanel1.SuspendLayout()
        Me.TableLayoutPanel2.SuspendLayout()
        Me.FlowLayoutPanel1.SuspendLayout()
        Me.TableLayoutPanel3.SuspendLayout()
        Me.SuspendLayout()
        '
        'TableLayoutPanel1
        '
        Me.TableLayoutPanel1.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.TableLayoutPanel1.ColumnCount = 2
        Me.TableLayoutPanel1.ColumnStyles.Add(New System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 50.0!))
        Me.TableLayoutPanel1.ColumnStyles.Add(New System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 50.0!))
        Me.TableLayoutPanel1.Controls.Add(Me.OK_Button, 0, 0)
        Me.TableLayoutPanel1.Controls.Add(Me.Cancel_Button, 1, 0)
        Me.TableLayoutPanel1.Location = New System.Drawing.Point(155, 110)
        Me.TableLayoutPanel1.Margin = New System.Windows.Forms.Padding(0)
        Me.TableLayoutPanel1.Name = "TableLayoutPanel1"
        Me.TableLayoutPanel1.RowCount = 1
        Me.TableLayoutPanel1.RowStyles.Add(New System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 50.0!))
        Me.TableLayoutPanel1.Size = New System.Drawing.Size(179, 36)
        Me.TableLayoutPanel1.TabIndex = 0
        '
        'OK_Button
        '
        Me.OK_Button.Dock = System.Windows.Forms.DockStyle.Fill
        Me.OK_Button.Location = New System.Drawing.Point(3, 3)
        Me.OK_Button.Name = "OK_Button"
        Me.OK_Button.Size = New System.Drawing.Size(83, 30)
        Me.OK_Button.TabIndex = 1
        Me.OK_Button.Text = "OK"
        '
        'Cancel_Button
        '
        Me.Cancel_Button.DialogResult = System.Windows.Forms.DialogResult.Cancel
        Me.Cancel_Button.Dock = System.Windows.Forms.DockStyle.Fill
        Me.Cancel_Button.Location = New System.Drawing.Point(92, 3)
        Me.Cancel_Button.Name = "Cancel_Button"
        Me.Cancel_Button.Size = New System.Drawing.Size(84, 30)
        Me.Cancel_Button.TabIndex = 2
        Me.Cancel_Button.Text = "Cancel"
        '
        'TableLayoutPanel2
        '
        Me.TableLayoutPanel2.ColumnCount = 3
        Me.TableLayoutPanel2.ColumnStyles.Add(New System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Absolute, 20.0!))
        Me.TableLayoutPanel2.ColumnStyles.Add(New System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 100.0!))
        Me.TableLayoutPanel2.ColumnStyles.Add(New System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Absolute, 20.0!))
        Me.TableLayoutPanel2.Controls.Add(Me.TextBox1, 1, 1)
        Me.TableLayoutPanel2.Controls.Add(Me.TableLayoutPanel1, 1, 2)
        Me.TableLayoutPanel2.Controls.Add(Me.TableLayoutPanel3, 1, 0)
        Me.TableLayoutPanel2.Dock = System.Windows.Forms.DockStyle.Fill
        Me.TableLayoutPanel2.Location = New System.Drawing.Point(0, 0)
        Me.TableLayoutPanel2.Margin = New System.Windows.Forms.Padding(0)
        Me.TableLayoutPanel2.Name = "TableLayoutPanel2"
        Me.TableLayoutPanel2.Padding = New System.Windows.Forms.Padding(0, 0, 0, 15)
        Me.TableLayoutPanel2.RowCount = 3
        Me.TableLayoutPanel2.RowStyles.Add(New System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 50.0!))
        Me.TableLayoutPanel2.RowStyles.Add(New System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 50.0!))
        Me.TableLayoutPanel2.RowStyles.Add(New System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 40.0!))
        Me.TableLayoutPanel2.Size = New System.Drawing.Size(354, 161)
        Me.TableLayoutPanel2.TabIndex = 2
        '
        'TextBox1
        '
        Me.TextBox1.Dock = System.Windows.Forms.DockStyle.Top
        Me.TextBox1.Location = New System.Drawing.Point(23, 56)
        Me.TextBox1.Name = "TextBox1"
        Me.TextBox1.Text = "0"
        Me.TextBox1.Size = New System.Drawing.Size(308, 20)
        Me.TextBox1.TabIndex = 0
        '
        'RadioButton1
        '
        Me.RadioButton1.AutoSize = True
        Me.RadioButton1.Checked = True
        Me.RadioButton1.Location = New System.Drawing.Point(3, 3)
        Me.RadioButton1.Name = "RadioButton1"
        Me.RadioButton1.Size = New System.Drawing.Size(53, 17)
        Me.RadioButton1.TabIndex = 3
        Me.RadioButton1.TabStop = True
        Me.RadioButton1.Text = "Offset"
        Me.RadioButton1.UseVisualStyleBackColor = True
        '
        'FlowLayoutPanel1
        '
        Me.FlowLayoutPanel1.Controls.Add(Me.RadioButton1)
        Me.FlowLayoutPanel1.Controls.Add(Me.RadioButton2)
        Me.FlowLayoutPanel1.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.FlowLayoutPanel1.Location = New System.Drawing.Point(0, 28)
        Me.FlowLayoutPanel1.Margin = New System.Windows.Forms.Padding(0)
        Me.FlowLayoutPanel1.Name = "FlowLayoutPanel1"
        Me.FlowLayoutPanel1.Size = New System.Drawing.Size(267, 25)
        Me.FlowLayoutPanel1.TabIndex = 5
        '
        'RadioButton2
        '
        Me.RadioButton2.AutoSize = True
        Me.RadioButton2.Location = New System.Drawing.Point(62, 3)
        Me.RadioButton2.Name = "RadioButton2"
        Me.RadioButton2.Size = New System.Drawing.Size(83, 17)
        Me.RadioButton2.TabIndex = 4
        Me.RadioButton2.Text = "Line number"
        Me.RadioButton2.UseVisualStyleBackColor = True
        '
        'CheckBox1
        '
        Me.CheckBox1.AutoSize = True
        Me.CheckBox1.Checked = True
        Me.CheckBox1.CheckState = System.Windows.Forms.CheckState.Checked
        Me.CheckBox1.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.CheckBox1.Location = New System.Drawing.Point(267, 33)
        Me.CheckBox1.Margin = New System.Windows.Forms.Padding(0, 0, 0, 3)
        Me.CheckBox1.Name = "CheckBox1"
        Me.CheckBox1.Size = New System.Drawing.Size(47, 17)
        Me.CheckBox1.TabIndex = 6
        Me.CheckBox1.Text = "Hex"
        Me.CheckBox1.UseVisualStyleBackColor = True
        '
        'TableLayoutPanel3
        '
        Me.TableLayoutPanel3.ColumnCount = 2
        Me.TableLayoutPanel3.ColumnStyles.Add(New System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 100.0!))
        Me.TableLayoutPanel3.ColumnStyles.Add(New System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Absolute, 47.0!))
        Me.TableLayoutPanel3.Controls.Add(Me.FlowLayoutPanel1, 0, 0)
        Me.TableLayoutPanel3.Controls.Add(Me.CheckBox1, 1, 0)
        Me.TableLayoutPanel3.Dock = System.Windows.Forms.DockStyle.Fill
        Me.TableLayoutPanel3.Location = New System.Drawing.Point(20, 0)
        Me.TableLayoutPanel3.Margin = New System.Windows.Forms.Padding(0)
        Me.TableLayoutPanel3.Name = "TableLayoutPanel3"
        Me.TableLayoutPanel3.RowCount = 1
        Me.TableLayoutPanel3.RowStyles.Add(New System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 100.0!))
        Me.TableLayoutPanel3.Size = New System.Drawing.Size(314, 53)
        Me.TableLayoutPanel3.TabIndex = 6
        '
        'GotoDialog
        '
        Me.AcceptButton = Me.OK_Button
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.CancelButton = Me.Cancel_Button
        Me.ClientSize = New System.Drawing.Size(354, 161)
        Me.Controls.Add(Me.TableLayoutPanel2)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog
        Me.MaximizeBox = False
        Me.MinimizeBox = False
        Me.Name = "GotoDialog"
        Me.ShowInTaskbar = False
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent
        Me.Text = "Goto...."
        Me.TableLayoutPanel1.ResumeLayout(False)
        Me.TableLayoutPanel2.ResumeLayout(False)
        Me.TableLayoutPanel2.PerformLayout()
        Me.FlowLayoutPanel1.ResumeLayout(False)
        Me.FlowLayoutPanel1.PerformLayout()
        Me.TableLayoutPanel3.ResumeLayout(False)
        Me.TableLayoutPanel3.PerformLayout()
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents TableLayoutPanel1 As System.Windows.Forms.TableLayoutPanel
    Friend WithEvents OK_Button As System.Windows.Forms.Button
    Friend WithEvents Cancel_Button As System.Windows.Forms.Button
    Friend WithEvents TableLayoutPanel2 As System.Windows.Forms.TableLayoutPanel
    Friend WithEvents TextBox1 As System.Windows.Forms.TextBox
    Friend WithEvents FlowLayoutPanel1 As System.Windows.Forms.FlowLayoutPanel
    Friend WithEvents RadioButton1 As System.Windows.Forms.RadioButton
    Friend WithEvents RadioButton2 As System.Windows.Forms.RadioButton
    Friend WithEvents TableLayoutPanel3 As System.Windows.Forms.TableLayoutPanel
    Friend WithEvents CheckBox1 As System.Windows.Forms.CheckBox
    Private Sub MyLoad(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        TextBox1.Focus()
    End Sub
    Private Sub OK_Button_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles OK_Button.Click
        Me.DialogResult = System.Windows.Forms.DialogResult.OK
        Me.Close()
    End Sub

    Private Sub Cancel_Button_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Cancel_Button.Click
        Me.DialogResult = System.Windows.Forms.DialogResult.Cancel
        Me.Close()
    End Sub

    Private Sub CheckBox1_Checked(sender As Object, e As EventArgs) Handles CheckBox1.CheckedChanged
        If CheckBox1.Checked Then
            Dim tx As String = TextBox1.Text
            Dim v As Long = 0
            If IsNumeric(tx) Then
                v = tx
            End If
            If v < 0 Then v = 0
            Me.TextBox1.Text = Hex(v) & "h"
        Else
            Dim tx As String = "&H" & TextBox1.Text
            If CStr(tx.Last).ToLower = "h" Then
                tx = tx.Remove(tx.Length - 1)
            End If
            Dim v As Long = 0
            If IsNumeric(tx) Then
                v = tx
            End If
            If v < 0 Then v = 0
            Me.TextBox1.Text = v
        End If
    End Sub
    Private Sub RadioButton1_Checked(sender As Object, e As EventArgs) Handles RadioButton1.CheckedChanged
        Dim v As Long = 0
        Dim tx As String = ""
        If CheckBox1.Checked Then
            tx = "&H" & TextBox1.Text
            If CStr(tx.Last).ToLower = "h" Then
                tx = tx.Remove(tx.Length - 1)
            End If
        Else
            tx = TextBox1.Text
        End If
        If IsNumeric(tx) Then
            v = tx
        End If
        If v < 0 Then v = 0

        If RadioButton1.Checked Then
            v = v * col_count
        Else
            v = v / col_count
        End If

        If CheckBox1.Checked Then
            Me.TextBox1.Text = Hex(v) & "h"
        Else
            Me.TextBox1.Text = v
        End If
    End Sub
End Class

Public Enum ValueModes As Integer
    Offset = 1
    Line = 2
End Enum