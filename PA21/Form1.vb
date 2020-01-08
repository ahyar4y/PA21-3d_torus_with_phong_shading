Public Class Form1
    Dim bmp As Bitmap
    Dim img As Graphics
    Dim rMatrix(3, 3) As Double
    Dim sMatrix(,) As Double = {{100, 0, 0, 0}, {0, 100, 0, 0}, {0, 0, 100, 0}, {0, 0, 0, 1}}
    Dim torus As Torus3D
    Friend lightSource As Vector3D
    Friend viewer As Vector3D
    Public centerX, centerY, centerZ As Integer
    Dim rX, rY, rZ As Integer
    Dim tX, tY, tZ As Double
    Public ka, ia, kd, ks, il As Double
    Public specExp As Integer

    Private Sub PictureBox1_MouseMove(sender As Object, e As MouseEventArgs) Handles PictureBox1.MouseMove
        Label5.Text = "X: " + (e.X - centerX).ToString
        Label6.Text = "Y: " + (-1 * (e.Y - centerY)).ToString
    End Sub

    Private Sub Form1_Shown(sender As Object, e As EventArgs) Handles MyBase.Shown
        bmp = New Bitmap(PictureBox1.Width, PictureBox1.Height)
        img = Graphics.FromImage(bmp)
        viewer = New Vector3D(0.0, 0.0, 10.0)
        lightSource = New Vector3D(CDbl(NumericUpDown17.Text), CDbl(NumericUpDown18.Text), CDbl(NumericUpDown19.Text))
        lightSource = MultiplyWithMatrix(lightSource, sMatrix)
        centerX = PictureBox1.Width / 2
        centerY = PictureBox1.Height / 2
        centerZ = 0.0

        torus = New Torus3D(0.0, 0.0, 0.0, 1, 0.5, 30, 30)
        ka = CDbl(NumericUpDown11.Text)
        ia = CDbl(NumericUpDown12.Text)
        kd = CDbl(NumericUpDown13.Text)
        ks = CDbl(NumericUpDown14.Text)
        specExp = CInt(NumericUpDown15.Text)
        il = CDbl(NumericUpDown16.Text)

        DrawObject(img, torus)
        PictureBox1.Image = bmp
        img.Dispose()
    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        rX = CInt(NumericUpDown5.Text)
        rY = CInt(NumericUpDown6.Text)
        rZ = CInt(NumericUpDown7.Text)

        SetMatrixRow(rMatrix, 0, Math.Cos(rY * Math.PI / 180) * Math.Cos(rZ * Math.PI / 180), Math.Cos(rY * Math.PI / 180) * Math.Sin(rZ * Math.PI / 180), -Math.Sin(rY * Math.PI / 180), 0)
        SetMatrixRow(rMatrix, 1, Math.Sin(rX * Math.PI / 180) * Math.Sin(rY * Math.PI / 180) * Math.Cos(rZ * Math.PI / 180) + Math.Cos(rX * Math.PI / 180) * -Math.Sin(rZ * Math.PI / 180), Math.Sin(rX * Math.PI / 180) * Math.Sin(rY * Math.PI / 180) * Math.Sin(rZ * Math.PI / 180) + Math.Cos(rX * Math.PI / 180) * Math.Cos(rZ * Math.PI / 180), Math.Sin(rX * Math.PI / 180) * Math.Cos(rY * Math.PI / 180), 0)
        SetMatrixRow(rMatrix, 2, Math.Cos(rX * Math.PI / 180) * Math.Sin(rY * Math.PI / 180) * Math.Cos(rZ * Math.PI / 180) + -Math.Sin(rX * Math.PI / 180) * -Math.Sin(rZ * Math.PI / 180), Math.Cos(rX * Math.PI / 180) * Math.Sin(rY * Math.PI / 180) * Math.Sin(rZ * Math.PI / 180) + -Math.Sin(rX * Math.PI / 180) * Math.Cos(rZ * Math.PI / 180), Math.Cos(rX * Math.PI / 180) * Math.Cos(rY * Math.PI / 180), 0)
        SetMatrixRow(rMatrix, 3, 0, 0, 0, 1)

        Dim mesh As New Mesh3D(torus.m, torus.n)
        Dim mesh2 As New Mesh3D(torus.m, torus.n)
        For i = 0 To torus.m
            For j = 0 To torus.n
                mesh.v(i, j).x = torus.mesh.n(i, j).x * rMatrix(0, 0) + torus.mesh.n(i, j).y * rMatrix(1, 0) + torus.mesh.n(i, j).z * rMatrix(2, 0)
                mesh.v(i, j).y = torus.mesh.n(i, j).x * rMatrix(0, 1) + torus.mesh.n(i, j).y * rMatrix(1, 1) + torus.mesh.n(i, j).z * rMatrix(2, 1)
                mesh.v(i, j).z = torus.mesh.n(i, j).x * rMatrix(0, 2) + torus.mesh.n(i, j).y * rMatrix(1, 2) + torus.mesh.n(i, j).z * rMatrix(2, 2)
                mesh.n(i, j).x = torus.smallCenter.n(i, j).x * rMatrix(0, 0) + torus.smallCenter.n(i, j).y * rMatrix(1, 0) + torus.smallCenter.n(i, j).z * rMatrix(2, 0)
                mesh.n(i, j).y = torus.smallCenter.n(i, j).x * rMatrix(0, 1) + torus.smallCenter.n(i, j).y * rMatrix(1, 1) + torus.smallCenter.n(i, j).z * rMatrix(2, 1)
                mesh.n(i, j).z = torus.smallCenter.n(i, j).x * rMatrix(0, 2) + torus.smallCenter.n(i, j).y * rMatrix(1, 2) + torus.smallCenter.n(i, j).z * rMatrix(2, 2)
                mesh2.v(i, j) = GetVertexNormal(MultiplyWithMatrix(mesh.n(i, j), sMatrix), torus.minorR * sMatrix(0, 0), MultiplyWithMatrix(mesh.v(i, j), sMatrix))
            Next
        Next
        torus.mesh.n = mesh.v
        torus.smallCenter.n = mesh.n
        _vNormal.v = mesh2.v

        img = Graphics.FromImage(bmp)
        DrawObject(img, torus)
        PictureBox1.Image = bmp
        img.Dispose()
    End Sub

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        tX = CDbl(NumericUpDown8.Text)
        tY = CDbl(NumericUpDown9.Text)
        tZ = CDbl(NumericUpDown10.Text)

        torus.center.x += tX
        torus.center.y += tY
        torus.center.z += tZ

        Dim mesh As New Mesh3D(torus.m, torus.n)
        Dim mesh2 As New Mesh3D(torus.m, torus.n)
        For i = 0 To torus.m
            For j = 0 To torus.n
                mesh.v(i, j).x = torus.mesh.n(i, j).x + tX
                mesh.v(i, j).y = torus.mesh.n(i, j).y + tY
                mesh.v(i, j).z = torus.mesh.n(i, j).z + tZ
                mesh.n(i, j).x = torus.smallCenter.n(i, j).x + tX
                mesh.n(i, j).y = torus.smallCenter.n(i, j).y + tY
                mesh.n(i, j).z = torus.smallCenter.n(i, j).z + tZ
                mesh2.v(i, j) = GetVertexNormal(MultiplyWithMatrix(mesh.n(i, j), sMatrix), torus.minorR * sMatrix(0, 0), MultiplyWithMatrix(mesh.v(i, j), sMatrix))
            Next
        Next
        torus.mesh.n = mesh.v
        torus.smallCenter.n = mesh.n
        _vNormal.v = mesh2.v

        img = Graphics.FromImage(bmp)
        DrawObject(img, torus)
        PictureBox1.Image = bmp
        img.Dispose()
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        ka = CDbl(NumericUpDown11.Text)
        ia = CDbl(NumericUpDown12.Text)
        kd = CDbl(NumericUpDown13.Text)
        ks = CDbl(NumericUpDown14.Text)
        specExp = CInt(NumericUpDown15.Text)
        il = CDbl(NumericUpDown16.Text)
        lightSource = New Vector3D(CDbl(NumericUpDown17.Text), CDbl(NumericUpDown18.Text), CDbl(NumericUpDown19.Text))
        lightSource = MultiplyWithMatrix(lightSource, sMatrix)

        img = Graphics.FromImage(bmp)
        DrawObject(img, torus)
        PictureBox1.Image = bmp
        img.Dispose()
    End Sub
End Class
