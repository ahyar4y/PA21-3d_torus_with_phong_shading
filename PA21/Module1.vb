Module Module1
    Public _vNormal As Mesh3D
    Dim St(,) As Double = {{100, 0, 0, 0}, {0, 100, 0, 0}, {0, 0, 100, 0}, {0, 0, 0, 1}}

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

        Public Sub MultiplyBy(n As Double)
            x *= n
            y *= n
            z *= n
        End Sub

        Public Function DotProduct(v As Vector3D) As Double
            Return Me.x * v.x + Me.y * v.y + Me.z * v.z
        End Function

        Public Function VectorLength() As Double
            Return Math.Sqrt(DotProduct(Me))
        End Function

        Public Function Plus(v As Vector3D) As Vector3D
            Dim vOut As New Vector3D
            vOut.x = Me.x + v.x
            vOut.y = Me.y + v.y
            vOut.z = Me.z + v.z

            Return vOut
        End Function

        Public Function Add(v As Vector3D) As Vector3D
            Dim vOut As New Vector3D
            vOut.x = x + v.x
            vOut.y = y + v.y
            vOut.z = z + v.z

            Return vOut
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

        Public Sub New(_rows As Integer, _cols As Integer)
            Me.rows = _rows
            Me.cols = _cols
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
        Public center As Vector3D
        Public smallCenter As Mesh3D
        Public majorR, minorR As Double
        Public m, n As Integer
        Public c_theta, s_theta, c_phi, s_phi As Double
        Public mesh As Mesh3D

        Public Sub New(_x As Double, _y As Double, _z As Double, _majorR As Double, _minorR As Double, _m As Integer, _n As Integer)
            center = New Vector3D(_x, _y, _z)
            Me.majorR = _majorR
            Me.minorR = _minorR
            Me.m = _m
            Me.n = _n
            InitMesh()
        End Sub

        Private Sub InitMesh()
            mesh = New Mesh3D(m, n)
            _vNormal = New Mesh3D(m, n)
            smallCenter = New Mesh3D(m, n)
            FillMesh()
        End Sub

        Public Sub FillMesh()
            Dim phi, theta As Double
            Dim d_phi As Integer = Math.Ceiling(360 / CDbl(m))
            Dim d_theta As Integer = Math.Ceiling(360 / CDbl(n))

            theta = -Math.PI
            phi = -Math.PI

            For i = 0 To m
                phi += d_phi

                c_phi = CDbl(Math.Cos(phi * Math.PI / 180))
                s_phi = CDbl(Math.Sin(phi * Math.PI / 180))

                For j = 0 To n
                    theta += d_theta

                    c_theta = CDbl(Math.Cos(theta * Math.PI / 180))
                    s_theta = CDbl(Math.Sin(theta * Math.PI / 180))

                    mesh.v(i, j).x = (majorR + minorR * c_theta) * c_phi
                    mesh.v(i, j).y = (majorR + minorR * c_theta) * s_phi
                    mesh.v(i, j).z = minorR * s_theta
                    smallCenter.v(i, j).x = center.x + majorR * c_phi
                    smallCenter.v(i, j).y = center.y + majorR * s_phi
                    smallCenter.v(i, j).z = center.z
                    _vNormal.v(i, j) = GetVertexNormal(MultiplyWithMatrix(smallCenter.v(i, j), St), minorR * St(0, 0), MultiplyWithMatrix(mesh.v(i, j), St))
                Next
            Next
            mesh.n = mesh.v
            smallCenter.n = smallCenter.v
        End Sub
    End Class

    Class PolygonData
        Structure PEdge
            Public p1, p2 As Vector3D
            Public yMin, xYMin, yMax, dx, dy As Double
            Public eVNormal() As Vector3D
        End Structure

        Public pPoint(2) As Vector3D
        Public pEdges(2) As PEdge
        Public pYMin, pYMax As Double
        Public pZMin As Double
        Public pNormal As Vector3D
        Public vNormal(2) As Vector3D
        Public d, zx, zy As Double
        Public a, b, c As Double

        Public Sub New(torus As Torus3D, v0 As Vector3D, v1 As Vector3D, v2 As Vector3D, vN() As Vector3D)
            pPoint = {v0, v1, v2}

            pNormal = ComputeTriangleNormal(v0, v1, v2)
            vNormal = vN

            pYMax = pPoint(0).y
            pYMin = pPoint(0).y
            pZMin = pPoint(0).z

            For i = 0 To 2
                ReDim pEdges(i).eVNormal(1)

                If pYMin > pPoint(i).y Then
                    pYMin = pPoint(i).y
                    pZMin = pPoint(i).z
                End If

                If pYMax < pPoint(i).y Then
                    pYMax = pPoint(i).y
                End If
            Next

            If pNormal.z = 0 Then
                zx = 0
                zy = 0
            Else
                zx = pNormal.x / pNormal.z
                zy = pNormal.y / pNormal.z
            End If

            'For i = 0 To 2
            '    Console.WriteLine(vNormal(i).x.ToString + " " + vNormal(i).y.ToString + " " + vNormal(i).z.ToString)
            'Next

            'Console.WriteLine("Polygon Y Min: " & pYMin.ToString)
            'Console.WriteLine("Polygon Y Max: " & pYMax.ToString)
            'Console.WriteLine(pNormal.x.ToString + " " + pNormal.y.ToString + " " + pNormal.z.ToString)
        End Sub

        Sub SetEdges()
            Dim temp As Vector3D
            For i = 0 To 1
                pEdges(i).p1 = pPoint(i)
                pEdges(i).p2 = pPoint(i + 1)
                pEdges(i).eVNormal(0) = vNormal(i)
                pEdges(i).eVNormal(1) = vNormal(i + 1)
            Next

            pEdges(2).p1 = pPoint(2)
            pEdges(2).p2 = pPoint(0)
            pEdges(2).eVNormal(0) = vNormal(2)
            pEdges(2).eVNormal(1) = vNormal(0)

            For i = 0 To 2
                pEdges(i).yMax = pEdges(i).p2.y
                pEdges(i).yMin = pEdges(i).p1.y
                pEdges(i).xYMin = pEdges(i).p1.x

                If pEdges(i).yMax < pEdges(i).p1.y Then
                    pEdges(i).yMax = pEdges(i).p1.y
                    pEdges(i).yMin = pEdges(i).p2.y
                    pEdges(i).xYMin = pEdges(i).p2.x
                    temp = pEdges(i).eVNormal(0)
                    pEdges(i).eVNormal(0) = pEdges(i).eVNormal(1)
                    pEdges(i).eVNormal(1) = temp
                End If

                pEdges(i).dx = pEdges(i).p2.x - pEdges(i).p1.x
                pEdges(i).dy = pEdges(i).p2.y - pEdges(i).p1.y

                If pEdges(i).dy < 0 Then
                    pEdges(i).dy = pEdges(i).dy * -1
                    pEdges(i).dx = pEdges(i).dx * -1
                End If
            Next
        End Sub
    End Class

    Class SETElmt
        Public yMin, yMax, xYMin, zYmin, dx, dy, carry As Integer
        Public vNormal(1) As Vector3D
        Public SETNext As SETElmt
    End Class

    Sub SetMatrixRow(ByRef matrix(,) As Double, i As Integer, a As Double, b As Double, c As Double, d As Double)
        matrix(i, 0) = a
        matrix(i, 1) = b
        matrix(i, 2) = c
        matrix(i, 3) = d
    End Sub

    Function MultiplyWithMatrix(v As Vector3D, m(,) As Double) As Vector3D
        Dim vOut As New Vector3D
        vOut.x = CInt(v.x * m(0, 0) + v.y * m(1, 0) + v.z * m(2, 0))
        vOut.y = CInt(v.x * m(0, 1) + v.y * m(1, 1) + v.z * m(2, 1))
        vOut.z = CInt(v.x * m(0, 2) + v.y * m(1, 2) + v.z * m(2, 2))

        Return vOut
    End Function

    Sub CreateSET(img As Graphics, center As Vector3D, p As PolygonData)
        Dim SETrange As Integer = p.pYMax - p.pYMin + 1
        Dim pSET(SETrange - 1) As SETElmt
        Dim temp As SETElmt

        For i = 0 To SETrange - 1
            pSET(i) = New SETElmt
            pSET(i).yMin = Integer.MaxValue
        Next

        p.SetEdges()

        For i = 0 To 2
            If pSET(p.pEdges(i).yMin - p.pYMin).yMin = Integer.MaxValue Then
                temp = InsertSET(p, i)
                temp.SETNext = Nothing
                If temp.dy <> 0 Then
                    pSET(p.pEdges(i).yMin - p.pYMin) = temp
                End If
            Else
                temp = InsertSET(p, i)
                temp.SETNext = pSET(p.pEdges(i).yMin - p.pYMin)
                If temp.dy <> 0 Then
                    pSET(p.pEdges(i).yMin - p.pYMin) = temp
                End If
            End If
        Next

        'For i = 0 To SETrange - 1
        '    Console.WriteLine(i.ToString + " " + pSET(i).yMin.ToString + " " + pSET(i).yMax.ToString + " " + pSET(i).xYMin.ToString + " " + pSET(i).dx.ToString + " " + pSET(i).dy.ToString)
        '    'Console.WriteLine(pSET(i).vNormal(0).x.ToString + " " + pSET(i).vNormal(0).y.ToString + " " + pSET(i).vNormal(0).z.ToString)
        'Next

        FillPolygon(img, center, p, pSET)
    End Sub

    Function InsertSET(p As PolygonData, i As Integer) As SETElmt
        Dim temp As SETElmt = New SETElmt With {
            .yMin = p.pEdges(i).yMin,
            .yMax = p.pEdges(i).yMax,
            .xYMin = p.pEdges(i).xYMin,
            .dx = p.pEdges(i).dx,
            .dy = p.pEdges(i).dy,
            .carry = 0,
            .vNormal = p.pEdges(i).eVNormal
        }

        Return temp
    End Function

    Sub FillPolygon(img As Graphics, center As Vector3D, pNew As PolygonData, pSET() As SETElmt)
        Dim AEL As SETElmt
        Dim cur As SETElmt = AEL
        Dim prev As SETElmt = Nothing
        Dim nA, nB, nx As Vector3D
        Dim pixelColor As Color
        Dim x0 As Double = pSET(0).xYMin
        Dim zp As Double = pNew.pZMin
        Dim zpNew As Double = zp

        For i = 0 To pSET.Length - 1
            While cur IsNot Nothing
                If cur.yMax - pNew.pYMin = i Then
                    If prev IsNot Nothing Then
                        prev.SETNext = cur.SETNext
                    Else
                        AEL = cur.SETNext
                    End If
                Else
                    prev = cur
                End If

                cur = cur.SETNext
            End While

            cur = AEL

            If cur IsNot Nothing Then
                While cur.SETNext IsNot Nothing
                    cur = cur.SETNext
                End While

                If pSET(i).yMin = Integer.MaxValue Then
                    cur.SETNext = Nothing
                Else
                    cur.SETNext = pSET(i)
                End If
            Else
                If pSET(i).yMin = Integer.MaxValue Then
                    AEL = Nothing
                Else
                    AEL = pSET(i)
                End If
            End If

            AEL = SortSET(AEL)
            cur = AEL

            While cur IsNot Nothing AndAlso cur.SETNext IsNot Nothing
                nA = InterpolateNormal(cur.vNormal(1), cur.vNormal(0), cur.yMax, cur.yMin, i + pNew.pYMin)
                nB = InterpolateNormal(cur.SETNext.vNormal(1), cur.SETNext.vNormal(0), cur.SETNext.yMax, cur.SETNext.yMin, i + pNew.pYMin)
                'Console.WriteLine((i + pNew.pYMin).ToString + " | " + cur.vNormal(0).x.ToString + " " + cur.vNormal(0).y.ToString + " " + cur.vNormal(0).z.ToString + " || " + cur.vNormal(1).x.ToString + " " + cur.vNormal(1).y.ToString + " " + cur.vNormal(1).z.ToString)
                'Console.WriteLine((i + pNew.pYMin).ToString + " | " + cur.SETNext.vNormal(0).x.ToString + " " + cur.SETNext.vNormal(0).y.ToString + " " + cur.SETNext.vNormal(0).z.ToString + " || " + cur.SETNext.vNormal(1).x.ToString + " " + cur.SETNext.vNormal(1).y.ToString + " " + cur.SETNext.vNormal(1).z.ToString)
                'Console.WriteLine(cur.yMax.ToString + " " + cur.yMin.ToString + " " + (i + pNew.pYMin).ToString + " " + ((cur.yMax - (i + pNew.pYMin)) / (cur.yMax - cur.yMin)).ToString)
                'Console.WriteLine((i + pNew.pYMin).ToString + " | " + nA.x.ToString + " " + nA.y.ToString + " " + nA.z.ToString + " || " + nB.x.ToString + " " + nB.y.ToString + " " + nB.z.ToString)
                If x0 > cur.xYMin Then
                    For k = x0 To cur.xYMin Step -1
                        zpNew += pNew.zx
                    Next
                End If

                For j = cur.xYMin To cur.SETNext.xYMin
                    If cur.SETNext.xYMin - cur.xYMin = 0 Then
                        'Console.WriteLine(j.ToString + " " + nA.x.ToString + " " + nA.y.ToString + " " + nA.z.ToString)
                        pixelColor = PhongIllumination(New Vector3D(j, i + pNew.pYMin, zpNew), nA)
                        'Console.WriteLine(j.ToString + " " + pixelColor.R.ToString + " " + pixelColor.G.ToString + " " + pixelColor.B.ToString)
                    Else
                        nx = InterpolateNormal(nB, nA, cur.SETNext.xYMin, cur.xYMin, j)
                        'Console.WriteLine(j.ToString + " " + nx.x.ToString + " " + nx.y.ToString + " " + nx.z.ToString)
                        pixelColor = PhongIllumination(New Vector3D(j, i + pNew.pYMin, zpNew), nx)
                        'Console.WriteLine(j.ToString + " " + pixelColor.R.ToString + " " + pixelColor.G.ToString + " " + pixelColor.B.ToString)
                    End If
                    'Console.WriteLine(j.ToString + " " + (i + pNew.pYMin).ToString + " " + zpNew.ToString)
                    img.DrawRectangle(New Pen(pixelColor), CInt(j + center.x), -CInt(i + pNew.pYMin) + CInt(center.y), 1, 1)
                    If j <> cur.SETNext.xYMin Then
                        zpNew -= pNew.zx
                    End If
                Next
                cur = cur.SETNext.SETNext
            End While

            zpNew = zp
            zpNew -= (pNew.zy * (i + 1))

            cur = AEL

            While cur IsNot Nothing
                cur.carry += cur.dx

                If cur.dx < 0 Then
                    While -2 * cur.carry >= cur.dy
                        cur.xYMin -= 1
                        cur.carry += cur.dy
                    End While
                Else
                    While 2 * cur.carry >= cur.dy
                        cur.xYMin += 1
                        cur.carry -= cur.dy
                    End While
                End If

                cur = cur.SETNext
            End While

            cur = AEL
            prev = Nothing
        Next
    End Sub

    Function SortSET(unsorted As SETElmt) As SETElmt
        Dim sorted, cur, prev, cur_new, prev_new As SETElmt
        Dim xMin As Integer
        Dim swapping As Boolean = True

        Do
            prev = Nothing
            cur = unsorted

            prev_new = prev
            cur_new = cur

            If cur IsNot Nothing Then
                xMin = cur.xYMin
            End If

            While cur IsNot Nothing
                If cur.xYMin < xMin Then
                    xMin = cur.xYMin
                    cur_new = cur
                    prev_new = prev
                End If

                prev = cur
                cur = cur.SETNext
            End While

            If cur_new IsNot Nothing Then
                If sorted Is Nothing Then
                    sorted = New SETElmt
                    sorted = cur_new
                Else
                    Dim temp As New SETElmt
                    temp = cur_new
                    sorted.SETNext = temp
                End If
            End If

            If cur_new IsNot Nothing Then
                If prev_new IsNot Nothing Then
                    prev_new.SETNext = cur_new.SETNext
                Else
                    unsorted = cur_new.SETNext
                End If
            End If

            If unsorted Is Nothing Then
                swapping = False
            End If
        Loop While swapping

        'Dim tempz As SETElmt = sorted
        'If tempz IsNot Nothing AndAlso tempz.SETNext IsNot Nothing Then
        '    Console.WriteLine(tempz.yMax.ToString + " " + tempz.xYMin.ToString + " " + tempz.dx.ToString + " " + tempz.dy.ToString + " " + tempz.SETNext.yMax.ToString + " " + tempz.SETNext.xYMin.ToString + " " + tempz.SETNext.dx.ToString + " " + tempz.SETNext.dy.ToString)
        'End If

        Return sorted
    End Function

    Function GetVertexNormal(center As Vector3D, minorR As Integer, v As Vector3D) As Vector3D
        Dim vNormal As New Vector3D

        vNormal.x = (v.x - center.x) / minorR
        vNormal.y = (v.y - center.y) / minorR
        vNormal.z = (v.z - center.z) / minorR
        'Console.WriteLine(vNormal.x.ToString + " " + vNormal.y.ToString + " " + vNormal.z.ToString)

        Return vNormal
    End Function

    Function InterpolateNormal(n1 As Vector3D, n2 As Vector3D, c1 As Double, c2 As Double, cp As Double) As Vector3D
        Dim np As New Vector3D
        np.x = n1.x - n2.x
        np.y = n1.y - n2.y
        np.z = n1.z - n2.z

        np.x *= ((c1 - cp) / (c1 - c2))
        np.y *= ((c1 - cp) / (c1 - c2))
        np.z *= ((c1 - cp) / (c1 - c2))

        np.x = n1.x - np.x
        np.y = n1.y - np.y
        np.z = n1.z - np.z
        'Console.WriteLine(np.x.ToString + " " + np.y.ToString + " " + np.z.ToString)

        Return np
    End Function

    Function PhongIllumination(p As Vector3D, pn As Vector3D) As Color
        Dim iAmb As Double
        Dim iDiff As Double
        Dim iSpec As Double

        Dim objColor As Vector3D
        Dim specColor As Vector3D

        Dim lightVector As New Vector3D
        lightVector.x = Form1.lightSource.x - p.x
        lightVector.y = Form1.lightSource.y - p.y
        lightVector.z = Form1.lightSource.z - p.z
        lightVector.Normalize()

        Dim viewVector As New Vector3D
        viewVector.x = Form1.viewer.x - p.x
        viewVector.y = Form1.viewer.y - p.y
        viewVector.z = Form1.viewer.z - p.z
        viewVector.Normalize()

        Dim reflVector As New Vector3D
        reflVector.x = pn.x * (lightVector.DotProduct(pn) * 2)
        reflVector.y = pn.y * (lightVector.DotProduct(pn) * 2)
        reflVector.z = pn.z * (lightVector.DotProduct(pn) * 2)
        reflVector = reflVector.Minus(lightVector)

        iAmb = Form1.ka * Form1.ia
        objColor = New Vector3D(255 * iAmb, 0 * iAmb, 0 * iAmb)

        iDiff = Form1.kd * Form1.il * lightVector.DotProduct(pn)
        If iDiff < 0.0 Then
            iDiff = 0
        End If
        objColor = objColor.Add(New Vector3D(255 * iDiff, 0 * iDiff, 0 * iDiff))

        iSpec = Form1.ks * Form1.il * Math.Pow(viewVector.DotProduct(reflVector), Form1.specExp)
        If iSpec < 0.0 Then
            iSpec = 0
        End If

        specColor = New Vector3D(255 * iSpec, 255 * iSpec, 255 * iSpec)
        objColor = objColor.Add(specColor)
        If objColor.x > 255 Then
            objColor.x = 255
        End If
        If objColor.y > 255 Then
            objColor.y = 255
        End If
        If objColor.z > 255 Then
            objColor.z = 255
        End If
        'Console.WriteLine(objColor.x.ToString + " " + objColor.y.ToString + " " + objColor.z.ToString)

        Return Color.FromArgb(objColor.x, objColor.y, objColor.z)
    End Function

    Sub DrawObject(img As Graphics, torus As Torus3D)
        Dim i, j As Integer
        Dim v0, v1, v2 As Vector3D
        Dim p As PolygonData
        Dim vN(2) As Vector3D
        Dim viewVector As New Vector3D(0.0, 0.0, -1.0)
        Dim triangleNormal As Vector3D

        img.Clear(Color.White)
        For i = 0 To torus.m - 1
            For j = 0 To torus.n - 1
                v0 = MultiplyWithMatrix(torus.mesh.n(i, j), St)
                v1 = MultiplyWithMatrix(torus.mesh.n(i, j + 1), St)
                v2 = MultiplyWithMatrix(torus.mesh.n(i + 1, j + 1), St)
                triangleNormal = ComputeTriangleNormal(v0, v1, v2)

                If Not BackFaceCulling(viewVector, triangleNormal) Then
                    vN(0) = _vNormal.v(i, j)
                    vN(1) = _vNormal.v(i, j + 1)
                    vN(2) = _vNormal.v(i + 1, j + 1)
                    p = New PolygonData(torus, v0, v1, v2, vN)

                    CreateSET(img, New Vector3D(Form1.centerX, Form1.centerY, Form1.centerZ), p)
                    'DrawTriangle(img, New Vector3D(cX, cY, cZ), v0, v1, v2)
                End If

                v0 = MultiplyWithMatrix(torus.mesh.n(i, j), St)
                v1 = MultiplyWithMatrix(torus.mesh.n(i + 1, j + 1), St)
                v2 = MultiplyWithMatrix(torus.mesh.n(i + 1, j), St)
                triangleNormal = ComputeTriangleNormal(v0, v1, v2)

                If Not BackFaceCulling(viewVector, triangleNormal) Then
                    vN(0) = _vNormal.v(i, j)
                    vN(1) = _vNormal.v(i + 1, j + 1)
                    vN(2) = _vNormal.v(i + 1, j)
                    p = New PolygonData(torus, v0, v1, v2, vN)

                    CreateSET(img, New Vector3D(Form1.centerX, Form1.centerY, Form1.centerZ), p)
                    'DrawTriangle(img, New Vector3D(cX, cY, cZ), v0, v1, v2)
                End If
            Next
        Next
    End Sub

    Function BackFaceCulling(viewerVector As Vector3D, _v As Vector3D)
        If viewerVector.DotProduct(_v) > 0.0 Then
            Return True
        Else
            Return False
        End If
    End Function

    Function ComputeTriangleNormal(v0 As Vector3D, v1 As Vector3D, v2 As Vector3D)
        Dim e0 As Vector3D = v2.Minus(v1)
        Dim e1 As Vector3D = v1.Minus(v0)
        Dim norm As Vector3D = e0.CrossProduct(e1)

        If norm.VectorLength > 0.000001 Then
            norm.Normalize()
        Else
            norm.SetVector(0.0, 0.0, 0.0)
        End If

        Return norm
    End Function

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

    Sub DrawTriangle(img As Graphics, center As Vector3D, v0 As Vector3D, v1 As Vector3D, v2 As Vector3D)
        img.DrawLine(New Pen(Color.Black), CInt(v0.x + center.x), -CInt(v0.y) + CInt(center.y), CInt(v1.x + center.x), -CInt(v1.y) + CInt(center.y))
        img.DrawLine(New Pen(Color.Black), CInt(v1.x + center.x), -CInt(v1.y) + CInt(center.y), CInt(v2.x + center.x), -CInt(v2.y) + CInt(center.y))
        img.DrawLine(New Pen(Color.Black), CInt(v2.x + center.x), -CInt(v2.y) + CInt(center.y), CInt(v0.x + center.x), -CInt(v0.y) + CInt(center.y))
        'Console.WriteLine(v0.x.ToString + " " + v0.y.ToString + " " + v0.z.ToString + " | " + v1.x.ToString + " " + v1.y.ToString + " " + v1.z.ToString + " | " + v2.x.ToString + " " + v2.y.ToString + " " + v2.z.ToString)
    End Sub
End Module
