Module Module1
    Sub InsertColumnMatrix(ByRef matrix(,) As Double, i As Integer, a As Double, b As Double, c As Double, d As Double)
        matrix(i, 0) = a
        matrix(i, 1) = b
        matrix(i, 2) = c
        matrix(i, 3) = d
    End Sub

    Class Vector3D
        Public x, y, z As Double

        Public Sub New()
            x = 0.0
            y = 0.0
            z = 0.0
        End Sub

        Public Sub New(_x As Double, _y As Double, _z As Double)
            x = _x
            y = _y
            z = _z
        End Sub

        Public Sub SetVector(_x As Double, _y As Double, _z As Double)
            x = _x
            y = _y
            z = _z
        End Sub


        Public Function DotProduct(v As Vector3D) As Double
            Return Me.x * v.x + Me.y * v.y + Me.z * v.z
        End Function

        Public Function VectorLength() As Double
            Return Math.Sqrt(DotProduct(Me))
        End Function

        Public Function Minus(v As Vector3D) As Vector3D
            Dim vOut As New Vector3D
            vOut.x = Me.x - v.x
            vOut.y = Me.y - v.y
            vOut.z = Me.z - v.z

            Return vOut
        End Function

        Public Function CrossProduct(v As Vector3D) As Vector3D
            Dim vOut As Vector3D = New Vector3D()

            vOut.x = Me.y * v.z - Me.z * v.y
            vOut.y = Me.z * v.x - Me.x * v.z
            vOut.z = Me.x * v.y - Me.y * v.x

            Return vOut
        End Function

        Public Sub Normalize()
            Dim mag As Double = Me.VectorLength

            If mag > 0.000001 Then
                Me.x /= mag
                Me.y /= mag
                Me.z /= mag
            Else
                Me.SetVector(0.0, 0.0, 0.0)
            End If
        End Sub
    End Class

    Class Mesh3D
        Public rows, cols As Integer
        Public v(,), n(,) As Vector3D

        Public Sub New(rows As Integer, cols As Integer)
            Me.rows = rows
            Me.cols = cols
            ReDim v(rows, cols)
            ReDim n(rows, cols)

            For i = 0 To rows
                For j = 0 To cols
                    v(i, j) = New Vector3D
                    n(i, j) = New Vector3D
                Next
            Next
        End Sub
    End Class

    Class Torus3D
        Dim center As Vector3D
        Dim majorR, minorR As Double
        Public m, n As Integer
        Public mesh As Mesh3D

        Public Sub New(_x As Double, _y As Double, _z As Double, majorR As Double, minorR As Double, m As Integer, n As Integer)
            center = New Vector3D(_x, _y, _z)
            Me.majorR = majorR
            Me.minorR = minorR
            Me.m = m
            Me.n = n
            InitMesh()
        End Sub

        Private Sub InitMesh()
            mesh = New Mesh3D(m, n)
            FillMesh()
        End Sub

        Public Sub FillMesh()
            Dim phi, theta As Double
            Dim d_phi = 360 / CDbl(n)
            Dim d_theta = 360 / CDbl(m)

            Dim c_theta, s_theta, c_phi, s_phi

            'Dim du As New Vector3D
            'Dim dv As New Vector3D
            theta = -Math.PI
            phi = -Math.PI

            For i = 0 To m
                theta += d_theta

                c_theta = CDbl(Math.Cos(theta * Math.PI / 180))
                s_theta = CDbl(Math.Sin(theta * Math.PI / 180))

                For j = 0 To n
                    phi += d_phi

                    c_phi = CDbl(Math.Cos(phi * Math.PI / 180))
                    s_phi = CDbl(Math.Sin(phi * Math.PI / 180))

                    mesh.v(i, j).x = center.x + (majorR + minorR * c_phi) * c_theta
                    mesh.v(i, j).y = center.y + (majorR + minorR * c_phi) * s_theta
                    mesh.v(i, j).z = center.z + minorR * s_phi
                Next
            Next
        End Sub

        Public Sub DrawTorus()

        End Sub
    End Class

    Function ComputeTriangleNormal(v0 As Vector3D, v1 As Vector3D, v2 As Vector3D)
        Dim e0 As Vector3D = v1.Minus(v2)
        Dim e1 As Vector3D = v0.Minus(v2)
        Dim norm As Vector3D = e0.CrossProduct(e1)

        If norm.VectorLength > 0.000001 Then
            norm.Normalize()
        Else
            norm.SetVector(0.0, 0.0, 0.0)
        End If

        Return norm
    End Function

    Sub DrawObject(img As Graphics, mesh As Mesh3D, m As Integer, n As Integer, viewVector As Vector3D)
        Dim i, j As Integer
        Dim v0, v1, v2 As Vector3D

        Dim triangleNormal As Vector3D

        img.Clear(Color.White)
        For i = 0 To m - 1
            For j = 0 To n - 1
                v0 = mesh.v(i, j)
                v1 = mesh.v(i, j + 1)
                v2 = mesh.v(i + 1, j + 1)
                triangleNormal = ComputeTriangleNormal(v0, v1, v2)

                If viewVector.DotProduct(triangleNormal) > 0.0 Then
                    DrawTriangle(img, v0, v1, v2)
                End If

                v0 = mesh.v(i, j)
                v1 = mesh.v(i + 1, j + 1)
                v2 = mesh.v(i + 1, j)
                triangleNormal = ComputeTriangleNormal(v0, v1, v2)

                If viewVector.DotProduct(triangleNormal) > 0.0 Then
                    DrawTriangle(img, v0, v1, v2)
                End If
            Next
        Next
    End Sub

    Function SortVertices(v0 As Vector3D, v1 As Vector3D, v2 As Vector3D) As Vector3D()
        Dim p() As Vector3D = {v0, v1, v2}
        Dim temp As Vector3D
        Dim j As Integer
        Dim swapped As Boolean = True

        While swapped
            swapped = False
            j += 1
            For i = 0 To 3 - j - 1
                If p(i).x > p(i + 1).x Then
                    temp = p(i)
                    p(i) = p(i + 1)
                    p(i + 1) = temp
                    swapped = True
                End If
            Next
        End While
        Return p
    End Function

    Sub DrawTriangle(img As Graphics, v0 As Vector3D, v1 As Vector3D, v2 As Vector3D)
        Dim p() As Vector3D = SortVertices(v0, v1, v2)

        img.DrawLine(New Pen(Color.Black), CInt(v0.x), CInt(v0.y), CInt(v1.x), CInt(v1.y))
        img.DrawLine(New Pen(Color.Black), CInt(v1.x), CInt(v1.y), CInt(v2.x), CInt(v2.y))
        img.DrawLine(New Pen(Color.Black), CInt(v2.x), CInt(v2.y), CInt(v0.x), CInt(v0.y))
    End Sub
End Module
