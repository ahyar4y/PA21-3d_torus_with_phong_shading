Public Class Form1
    Dim bmp As Bitmap
    Dim img As Graphics
    Dim wMatrix(3, 3), rMatrix(3, 3), vMatrix(3, 3), sMatrix(3, 3) As Double
    Dim torus As Torus3D
    Dim lightSource As Vector3D
    Dim viewer As Vector3D
    Dim centerX, centerY, centerZ As Integer
    Dim ka, ia, kd, ks, il As Double
    Dim n As Integer

    Private Sub NumericUpDown_ValueChanged(sender As Object, e As EventArgs) Handles NumericUpDown11.ValueChanged, NumericUpDown12.ValueChanged, NumericUpDown13.ValueChanged, NumericUpDown14.ValueChanged, NumericUpDown15.ValueChanged, NumericUpDown16.ValueChanged

    End Sub

    Private Sub PictureBox1_MouseMove(sender As Object, e As MouseEventArgs) Handles PictureBox1.MouseMove
        Label5.Text = "X: " + (e.X - centerX).ToString
        Label6.Text = "Y: " + (-1 * (e.Y - centerY)).ToString
    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        Dim rX As Integer = CInt(NumericUpDown5.Text)
        Dim rY As Integer = CInt(NumericUpDown6.Text)
        Dim rZ As Integer = CInt(NumericUpDown7.Text)

        SetMatrixRow(rMatrix, 0, Math.Cos(rY * Math.PI / 180) * Math.Cos(rZ * Math.PI / 180), Math.Cos(rY * Math.PI / 180) * Math.Sin(rZ * Math.PI / 180), -Math.Sin(rY * Math.PI / 180), 0)
        SetMatrixRow(rMatrix, 1, Math.Sin(rX * Math.PI / 180) * Math.Sin(rY * Math.PI / 180) * Math.Cos(rZ * Math.PI / 180) + Math.Cos(rX * Math.PI / 180) * -Math.Sin(rZ * Math.PI / 180), Math.Sin(rX * Math.PI / 180) * Math.Sin(rY * Math.PI / 180) * Math.Sin(rZ * Math.PI / 180) + Math.Cos(rX * Math.PI / 180) * Math.Cos(rZ * Math.PI / 180), Math.Sin(rX * Math.PI / 180) * Math.Cos(rY * Math.PI / 180), 0)
        SetMatrixRow(rMatrix, 2, Math.Cos(rX * Math.PI / 180) * Math.Sin(rY * Math.PI / 180) * Math.Cos(rZ * Math.PI / 180) + -Math.Sin(rX * Math.PI / 180) * -Math.Sin(rZ * Math.PI / 180), Math.Cos(rX * Math.PI / 180) * Math.Sin(rY * Math.PI / 180) * Math.Sin(rZ * Math.PI / 180) + -Math.Sin(rX * Math.PI / 180) * Math.Cos(rZ * Math.PI / 180), Math.Cos(rX * Math.PI / 180) * Math.Cos(rY * Math.PI / 180), 0)
        SetMatrixRow(rMatrix, 3, 0, 0, 0, 1)

        Dim mesh As New Mesh3D(torus.m, torus.n)
        For i = 0 To torus.m
            For j = 0 To torus.n
                mesh.v(i, j).x = CInt((torus.mesh.n(i, j).x) * rMatrix(0, 0) + torus.mesh.n(i, j).y * rMatrix(1, 0) + torus.mesh.n(i, j).z * rMatrix(2, 0))
                mesh.v(i, j).y = CInt((torus.mesh.n(i, j).x) * rMatrix(0, 1) + torus.mesh.n(i, j).y * rMatrix(1, 1) + torus.mesh.n(i, j).z * rMatrix(2, 1))
                mesh.v(i, j).z = CInt((torus.mesh.n(i, j).x) * rMatrix(0, 2) + torus.mesh.n(i, j).y * rMatrix(1, 2) + torus.mesh.n(i, j).z * rMatrix(2, 2))
            Next
        Next
        torus.mesh.n = mesh.v

        DrawObject(img, torus, viewer, lightSource, ka, ia, kd, ks, n, il)
        PictureBox1.Image = bmp
    End Sub

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        Dim tX As Integer = CInt(NumericUpDown8.Text)
        Dim tY As Integer = CInt(NumericUpDown9.Text)
        Dim tZ As Integer = CInt(NumericUpDown10.Text)

        Dim mesh As New Mesh3D(torus.m, torus.n)
        For i = 0 To torus.m
            For j = 0 To torus.n
                mesh.v(i, j).x = torus.mesh.n(i, j).x + tX
                mesh.v(i, j).y = torus.mesh.n(i, j).y + tY
                mesh.v(i, j).z = torus.mesh.n(i, j).z + tZ
            Next
        Next
        torus.mesh.n = mesh.v

        torus.center.x += tX
        torus.center.y -= tY
        torus.center.z += tZ

        DrawObject(img, torus, viewer, lightSource, ka, ia, kd, ks, n, il)
        PictureBox1.Image = bmp
    End Sub

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load

    End Sub

    Private Sub Form1_Shown(sender As Object, e As EventArgs) Handles MyBase.Shown
        bmp = New Bitmap(PictureBox1.Width, PictureBox1.Height)
        img = Graphics.FromImage(bmp)
        viewer = New Vector3D(0.0, 0.0, 4.0)
        lightSource = New Vector3D(CDbl(NumericUpDown17.Text), CDbl(NumericUpDown18.Text), CDbl(NumericUpDown19.Text))
        centerX = PictureBox1.Width / 2
        centerY = PictureBox1.Height / 2
        centerZ = 0.0

        torus = New Torus3D(centerX, centerY, centerZ, 100, 50, 10, 10)
        ka = CDbl(NumericUpDown11.Text)
        ia = CDbl(NumericUpDown12.Text)
        kd = CDbl(NumericUpDown13.Text)
        ks = CDbl(NumericUpDown14.Text)
        n = CInt(NumericUpDown15.Text)
        il = CDbl(NumericUpDown16.Text)

        DrawObject(img, torus, viewer, lightSource, ka, ia, kd, ks, n, il)
        PictureBox1.Image = bmp
    End Sub
End Class
