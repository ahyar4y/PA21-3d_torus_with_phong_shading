Module Module1
    'Dim clr() As Color = {Color.Red, Color.Green, Color.LightBlue, Color.Yellow, Color.Pink, Color.Violet}
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

        Public Function MultiplyBy(n As Double) As Vector3D
            Dim v As New Vector3D

            v.x *= n
            v.y *= n
            v.z *= n

            Return v
        End Function

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
            FillMesh()
        End Sub

        Public Sub FillMesh()
            Dim phi, theta As Double
            Dim d_phi As Integer = 360 / CDbl(n)
            Dim d_theta As Integer = 360 / CDbl(m)

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

                    mesh.v(i, j).x = CInt((majorR + minorR * c_phi) * c_theta)
                    mesh.v(i, j).y = CInt((majorR + minorR * c_phi) * s_theta)
                    mesh.v(i, j).z = CInt(minorR * s_phi)
                Next
            Next
            mesh.n = mesh.v
        End Sub
    End Class

    Class PolygonData
        Structure PEdge
            Public p1, p2 As Vector3D
            Public yMin, xYMin, yMax, dx, dy As Integer
            Public eVNormal() As Vector3D
        End Structure

        Public pPoint(2) As Vector3D
        Public pEdges(2) As PEdge
        Public pYMin, pYMax As Integer
        Public pNormal As Vector3D
        Public vNormal(2) As Vector3D
        Public pColor As Color

        Public Sub New(torus As Torus3D, v0 As Vector3D, v1 As Vector3D, v2 As Vector3D, vN() As Vector3D)
            pPoint = {v0, v1, v2}

            pNormal = ComputeTriangleNormal(v0, v1, v2)
            vNormal = vN

            pYMax = pPoint(0).y
            pYMin = pPoint(0).y

            For i = 0 To 2
                ReDim pEdges(i).eVNormal(1)

                If pYMin > pPoint(i).y Then
                    pYMin = pPoint(i).y
                End If

                If pYMax < pPoint(i).y Then
                    pYMax = pPoint(i).y
                End If
            Next

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
                'Console.WriteLine(pEdges(i).p2.y - pEdges(i).p1.y)

                If pEdges(i).dy < 0 Then
                    pEdges(i).dy = pEdges(i).dy * -1
                    pEdges(i).dx = pEdges(i).dx * -1
                End If
            Next
        End Sub
    End Class

    Class SETElmt
        Public yMin, yMax, xYMin, dx, dy, carry As Integer
        Public pNormal, vNormal(1) As Vector3D
        Public SETNext As SETElmt
    End Class

    Sub SetMatrixRow(ByRef matrix(,) As Double, i As Integer, a As Double, b As Double, c As Double, d As Double)
        matrix(i, 0) = a
        matrix(i, 1) = b
        matrix(i, 2) = c
        matrix(i, 3) = d
    End Sub

    Function maxY(_v() As Vector3D) As Integer
        Dim temp As Integer = _v(0).y

        For i = 0 To 2
            If temp < _v(i).y Then
                temp = _v(i).y
            End If
        Next

        Return temp
    End Function

    Function minY(_v() As Vector3D) As Integer
        Dim temp As Integer = _v(0).y

        For i = 0 To 2
            If temp > _v(i).y Then
                temp = _v(i).y
            End If
        Next

        Return temp
    End Function

    Sub CreateSET(img As Graphics, center As Vector3D, p As PolygonData, viewer As Vector3D, lightSource As Vector3D, ka As Double, ia As Double, kd As Double, ks As Double, n As Integer, il As Double)
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

        FillPolygon(img, center, p, pSET, viewer, lightSource, ka, ia, kd, ks, n, il)
    End Sub

    Function InsertSET(p As PolygonData, i As Integer) As SETElmt
        Dim temp As SETElmt = New SETElmt With {
            .yMin = p.pEdges(i).yMin,
            .yMax = p.pEdges(i).yMax,
            .xYMin = p.pEdges(i).xYMin,
            .dx = p.pEdges(i).dx,
            .dy = p.pEdges(i).dy,
            .carry = 0,
            .pNormal = p.pNormal,
            .vNormal = p.pEdges(i).eVNormal
        }

        Return temp
    End Function

    Sub FillPolygon(img As Graphics, center As Vector3D, pNew As PolygonData, pSET() As SETElmt, viewer As Vector3D, lightSource As Vector3D, ka As Double, ia As Double, kd As Double, ks As Double, n As Integer, il As Double)
        Dim AEL As SETElmt
        Dim cur As SETElmt = AEL
        Dim prev As SETElmt = Nothing
        Dim nA, nB, nx As Vector3D

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

            'While cur IsNot Nothing AndAlso cur.SETNext IsNot Nothing
            '    img.DrawLine(New Pen(Color.SkyBlue), CInt(cur.xYMin + center.x), -CInt(i + pNew.pYMin + center.y) + 2 * CInt(center.y), CInt(cur.SETNext.xYMin + center.x), -CInt(i + pNew.pYMin + center.y) + 2 * CInt(center.y))
            '    'Console.WriteLine(i.ToString + " " + (i + pNew.pYMin).ToString + " | " + cur.yMax.ToString + " " + cur.xYMin.ToString + " " + cur.dx.ToString + " " + cur.dy.ToString + " " + cur.carry.ToString + " || " + cur.SETNext.yMax.ToString + " " + cur.SETNext.xYMin.ToString + " " + cur.SETNext.dx.ToString + " " + cur.SETNext.dy.ToString + " " + cur.SETNext.carry.ToString)

            '    nA = InterpolateNormal(cur.vNormal(1), cur.vNormal(0), cur.yMax, cur.yMin, i + pNew.pYMin)
            '    nB = InterpolateNormal(cur.SETNext.vNormal(1), cur.SETNext.vNormal(0), cur.SETNext.yMax, cur.SETNext.yMin, i + pNew.pYMin)
            '    'Console.WriteLine(nA.x.ToString + " " + nA.y.ToString + " " + nA.z.ToString + " " + nB.x.ToString + " " + nB.y.ToString + " " + nB.z.ToString)
            '    'Console.WriteLine(cur.pNormal.x.ToString)

            '    For j = cur.xYMin To cur.SETNext.xYMin
            '        If cur.SETNext.xYMin - cur.xYMin <> 0 Then
            '            nx = InterpolateNormal(nB, nA, cur.SETNext.xYMin, cur.xYMin, j)
            '            'Console.WriteLine(nx.x.ToString + " " + nx.y.ToString + " " + nx.z.ToString)
            '            'PhongIllumination(viewer, lightSource, nx, cur.pNormal, ka, ia, kd, ks, n, il)
            '        End If
            '    Next

            '    cur = cur.SETNext.SETNext
            'End While

            While cur IsNot Nothing AndAlso cur.SETNext IsNot Nothing
                nA = InterpolateNormal(cur.vNormal(1), cur.vNormal(0), cur.yMax, cur.yMin, i + pNew.pYMin)
                nB = InterpolateNormal(cur.SETNext.vNormal(1), cur.SETNext.vNormal(0), cur.SETNext.yMax, cur.SETNext.yMin, i + pNew.pYMin)
                'Console.WriteLine(cur.vNormal(0).x.ToString + " " + cur.vNormal(0).y.ToString + " " + cur.vNormal(0).z.ToString)
                'Console.WriteLine(nA.x.ToString + " " + nA.y.ToString + " " + nA.z.ToString + " " + nB.x.ToString + " " + nB.y.ToString + " " + nB.z.ToString)

                For j = cur.xYMin To cur.SETNext.xYMin
                    If cur.SETNext.xYMin - cur.xYMin <> 0 Then
                        nx = InterpolateNormal(nB, nA, cur.SETNext.xYMin, cur.xYMin, j)
                        'Console.WriteLine(j.ToString + " " + nx.x.ToString + " " + nx.y.ToString + " " + nx.z.ToString)
                        PhongIllumination(viewer, lightSource, nx, cur.pNormal, ka, ia, kd, ks, n, il) 'WHAT HAPPENED HERE
                        'Console.WriteLine(cur.pNormal.x.ToString)
                    End If
                Next
                cur = cur.SETNext
            End While

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

    Function GetVertexNormal(center As Vector3D, majorR As Integer, minorR As Integer, c_theta As Double, s_theta As Double, v As Vector3D) As Vector3D
        Dim vNormal As New Vector3D

        vNormal.x = (v.x - majorR * c_theta) / minorR
        vNormal.y = (v.y - majorR * s_theta) / minorR
        vNormal.z = (v.z - 0) / minorR
        'Console.WriteLine(vNormal.x.ToString + " " + vNormal.y.ToString + " " + vNormal.z.ToString)

        Return vNormal
    End Function

    Function InterpolateNormal(n1 As Vector3D, n2 As Vector3D, c1 As Double, c2 As Double, cp As Double) As Vector3D
        Dim np As Vector3D
        np = n1.Minus(n1.Minus(n2).MultiplyBy((c1 - cp) / (c1 - c2)))
        'Console.WriteLine(np.x.ToString + " " + np.y.ToString + " " + np.z.ToString)
        Return np
    End Function

    Sub PhongIllumination(viewer As Vector3D, LightSource As Vector3D, p As Vector3D, pn As Vector3D, ka As Double, ia As Double, kd As Double, ks As Double, n As Integer, il As Double)
        Dim iAmb As Double
        Dim iDiff As Double
        Dim iSpec As Double
        Dim iTot As Double

        Dim lightVector As Vector3D = LightSource.Minus(p)
        lightVector.Normalize()

        Dim viewVector As Vector3D = viewer.Minus(p)
        viewVector.Normalize()

        Dim reflVector As Vector3D = pn.MultiplyBy(lightVector.DotProduct(pn) * 2).Minus(lightVector)


        iAmb = ka * ia
        iDiff = kd * il * lightVector.DotProduct(pn)
        If iDiff < 0.0 Then
            iDiff = 0.0
        End If
        iSpec = ks * il * Math.Pow(viewVector.DotProduct(reflVector), n)
        If iSpec < 0.0 Then
            iSpec = 0.0
        End If

        iTot = iAmb + iDiff + iSpec

        'Console.WriteLine(iTot.ToString)
    End Sub

    Sub DrawObject(img As Graphics, torus As Torus3D, viewer As Vector3D, lightSource As Vector3D, ka As Double, ia As Double, kd As Double, ks As Double, n As Integer, il As Double)
        Dim i, j As Integer
        Dim v0, v1, v2 As Vector3D
        Dim p As PolygonData
        Dim vNormal(2) As Vector3D
        Dim viewVector As New Vector3D(0.0, 0.0, -1.0)
        Dim triangleNormal As Vector3D

        img.Clear(Color.White)
        For i = 0 To torus.m - 1
            For j = 0 To torus.n - 1
                v0 = torus.mesh.n(i, j)
                v1 = torus.mesh.n(i, j + 1)
                v2 = torus.mesh.n(i + 1, j + 1)
                triangleNormal = ComputeTriangleNormal(v0, v1, v2)

                If Not BackFaceCulling(viewVector, triangleNormal) Then
                    vNormal(0) = GetVertexNormal(torus.center, torus.majorR, torus.minorR, torus.c_theta, torus.s_theta, v0)
                    vNormal(1) = GetVertexNormal(torus.center, torus.majorR, torus.minorR, torus.c_theta, torus.s_theta, v1)
                    vNormal(2) = GetVertexNormal(torus.center, torus.majorR, torus.minorR, torus.c_theta, torus.s_theta, v2)
                    p = New PolygonData(torus, v0, v1, v2, vNormal)

                    CreateSET(img, torus.center, p, viewer, lightSource, ka, ia, kd, ks, n, il)
                    DrawTriangle(img, torus.center, v0, v1, v2)
                End If

                v0 = torus.mesh.n(i, j)
                v1 = torus.mesh.n(i + 1, j + 1)
                v2 = torus.mesh.n(i + 1, j)
                triangleNormal = ComputeTriangleNormal(v0, v1, v2)

                If Not BackFaceCulling(viewVector, triangleNormal) Then
                    vNormal(0) = GetVertexNormal(torus.center, torus.majorR, torus.minorR, torus.c_theta, torus.s_theta, v0)
                    vNormal(1) = GetVertexNormal(torus.center, torus.majorR, torus.minorR, torus.c_theta, torus.s_theta, v1)
                    vNormal(2) = GetVertexNormal(torus.center, torus.majorR, torus.minorR, torus.c_theta, torus.s_theta, v2)
                    p = New PolygonData(torus, v0, v1, v2, vNormal)

                    CreateSET(img, torus.center, p, viewer, lightSource, ka, ia, kd, ks, n, il)
                    DrawTriangle(img, torus.center, v0, v1, v2)
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
        'Console.WriteLine(CInt(v0.x).ToString + " " + CInt(v0.y).ToString + " " + CInt(v0.z).ToString + " " + CInt(v1.x).ToString + " " + CInt(v1.y).ToString + " " + CInt(v1.z).ToString + " " + CInt(v2.x).ToString + " " + CInt(v2.y).ToString + " " + CInt(v2.z).ToString)
    End Sub
End Module
