Public Class Form1
    Dim img As Graphics
    Dim wMatrix(3, 3), rMatrix(3, 3), vMatrix(3, 3), sMatrix(3, 3) As Double
    Dim torus As Torus3D

    Private Sub PictureBox1_MouseMove(sender As Object, e As MouseEventArgs) Handles PictureBox1.MouseMove
        Label5.Text = "X: " + e.X.ToString
        Label6.Text = "Y: " + e.Y.ToString
    End Sub

    Dim centerX, centerY, centerZ, rX, rY, rZ As Double

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        rX = CDbl(NumericUpDown5.Text)
        rY = CDbl(NumericUpDown6.Text)
        rZ = CDbl(NumericUpDown7.Text)
        centerX = PictureBox1.Width / 2
        centerY = PictureBox1.Height / 2
        centerZ = 0

        torus = New Torus3D(centerX, centerY, centerZ, CDbl(NumericUpDown1.Text), CDbl(NumericUpDown2.Text), CInt(NumericUpDown3.Text), CInt(NumericUpDown4.Text))
        Dim viewVector As New Vector3D(0.0, 0.0, 1.0)

        DrawObject(img, torus.mesh, torus.m, torus.n, viewVector)
        'InsertColumnMatrix(wMatrix, 0, 1, 0, 0, 0)
        'InsertColumnMatrix(wMatrix, 1, 0, 1, 0, 0)
        'InsertColumnMatrix(wMatrix, 2, 0, 0, 1, 0)
        'InsertColumnMatrix(wMatrix, 3, 0, 0, 0, 1)

        InsertColumnMatrix(rMatrix, 0, Math.Cos(rZ * Math.PI / 180) * Math.Cos(rY * Math.PI / 180) + Math.Sin(rZ * Math.PI / 180) * Math.Sin(rY * Math.PI / 180) * Math.Sin(rX * Math.PI / 180), -Math.Sin(rZ * Math.PI / 180) * Math.Cos(rY * Math.PI / 180) + Math.Cos(rZ * Math.PI / 180) * Math.Sin(rY * Math.PI / 180) * Math.Sin(rX * Math.PI / 180), Math.Cos(rX * Math.PI / 180) * Math.Sin(rY * Math.PI / 180), 0)
        InsertColumnMatrix(rMatrix, 1, Math.Sin(rZ * Math.PI / 180) * Math.Cos(rX * Math.PI / 180), Math.Cos(rZ * Math.PI / 180) * Math.Cos(rX * Math.PI / 180), -Math.Sin(rX * Math.PI / 180), 0)
        InsertColumnMatrix(rMatrix, 2, Math.Cos(rZ * Math.PI / 180) * -Math.Sin(rY * Math.PI / 180) + Math.Sin(rZ * Math.PI / 180) * Math.Cos(rY * Math.PI / 180) * Math.Sin(rX * Math.PI / 180), -Math.Sin(rZ * Math.PI / 180) * -Math.Sin(rY * Math.PI / 180) + Math.Cos(rZ * Math.PI / 180) * Math.Cos(rY * Math.PI / 180) * Math.Sin(rX * Math.PI / 180), Math.Cos(rX * Math.PI / 180) * Math.Sin(rY * Math.PI / 180), 0)
        InsertColumnMatrix(rMatrix, 3, 0, 0, 0, 1)

        InsertColumnMatrix(vMatrix, 0, 1, 0, 0, 0)
        InsertColumnMatrix(vMatrix, 1, 0, 1, 0, 0)
        InsertColumnMatrix(vMatrix, 2, 0, 0, 0, 0)
        InsertColumnMatrix(vMatrix, 3, 0, 0, 0, 1)

        InsertColumnMatrix(sMatrix, 0, 1, 0, 0, PictureBox1.Width / 2)
        InsertColumnMatrix(sMatrix, 1, 0, 1, 0, PictureBox1.Height / 2)
        InsertColumnMatrix(sMatrix, 2, 0, 0, 1, 0)
        InsertColumnMatrix(sMatrix, 3, 0, 0, 0, 1)

        'torus.SetVertices(img, p1, p2, pI)
        'torus.DrawTorus(img, p1, p2, pI, wMatrix, rMatrix, vMatrix, sMatrix)
    End Sub

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        img = PictureBox1.CreateGraphics
    End Sub
End Class
