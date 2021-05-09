
object Util {
        def removeByIndex[A](vector:Vector[A] , index:Int): Vector[A] =
        {
                def loop(newVec:Vector[A],i:Int ): Vector[A] ={
                        if(i<vector.length){
                                if(index == i) {
                                        loop(newVec, i + 1)
                                }
                                else {
                                        loop(newVec:+vector(i) , i+1)
                                }
                        }
                        else{
                                newVec
                        }
                }
                loop(Vector(),0)
        }
        def findCo(normal: TimeSeries, index: Int): (String, String, Double) = {
                val feature = normal.features(index)
                val list = normal.features.drop(index + 1).map { (s: String) => (s, Util.pearson(Util.vectorToArray(normal.getValues(s).get), Util.vectorToArray(normal.getValues(feature).get)).abs) }
                val cFeature = Util.max[(String, Double)](list.toList, (d1: (String, Double), d2: (String, Double)) => math.floor(d1._2 - d2._2).toInt)
                (feature, cFeature._1, cFeature._2)
        }
        def fillMap(map : Map[String,Vector[Double]] ,vector: Vector[Double] , i : Int ,j : Int , lines: List[Array[String]]): Map[String,Vector[Double]] = {
                if(j < lines(0).length) {
                        if (i < lines.length) {
                                fillMap(map, vector :+ (lines(i)(j)).toDouble, i + 1, j , lines)
                        }
                        else {
                                fillMap(map ++ Map(lines(0)(j) -> vector) , Vector[Double]() , 1 , j+1,lines)
                        }
                }
                else {
                        map
                }
        }

        def max[A](list: List[A], f: (A, A) => Int): A = {
                def go(index: Int, max: A): A = {
                        if (index < list.length) {
                                if (f(max, list(index)) < 0) {
                                        go(index + 1, list(index))
                                }
                                else
                                        go(index + 1, max)
                        }
                        else
                                max
                }

                go(1, list.head)
        }

        def Flipmatrix(array: Array[Array[String]]): Array[Array[String]] = {
                val size = array(0).length

                def go(a: Array[Array[String]], index: Int): Array[Array[String]] = {
                        var vec: Array[String] = Array[String]()
                        if (index < size) {
                                array.foreach( //vec :+
                                        (l: Array[String]) => (vec = vec :+ l(index)))
                                go(a ++ Array(vec), index + 1)
                        }
                        else {
                                a
                        }
                }

                go(Array(), 0)
        }

        def map[A, B, C](aList: List[A], fA: A => B, fB: B => C): List[C] = {
                def go(n: Int, oldClist: List[C]): List[C] = {
                        if (n < 0) {
                                oldClist
                        }
                        else {
                                go(n - 1, fB(fA(aList(n))) :: oldClist)
                        }
                }

                go(aList.length - 1, List())
        }

        def isSorted[A](aList: List[A], func: (A, A) => Boolean): Boolean = {
                val length = aList.length
                if (length == 2) {
                        func(aList.head, aList(1))
                }
                else {
                        func(aList(length - 2), aList.last) && isSorted(aList.tail, func)
                }
        }

        def probs(xs: Array[Double]): Array[Double] = {
                map[Double, Double, Double](xs.toList, (a: Double) => xs.count((b: Double) => a == b), (count: Double) => count / xs.length).toArray
        }

        def entropy(array: Array[Double]): Double = {
                map[Double, Double, Double](array.distinct.toList, (a: Double) => array.count((b: Double) => a == b), (count: Double) => -(count / array.length) * (math.log(count / array.length) / math.log(2))).sum

        }

        def mu(xs: Array[Double]): Double = {
                map[Double, Double, Double](xs.distinct.toList, (a: Double) => xs.count((b: Double) => a == b) * a, (count: Double) => count / xs.length).sum
        }


        def variance(xs: Array[Double]): Double = {
                map[Double, Double, Double](xs.distinct.toList, (a: Double) => xs.count((b: Double) => a == b) * math.pow(a - mu(xs), 2), (count: Double) => count / xs.length).sum
        }

        def zscore(xs: Array[Double], score: Double): Double = {
                        (score - mu(xs)) / math.sqrt(variance(xs))

        }

        def pearson(xs: Array[Double], ys: Array[Double]): Double = {
                mu(map[(Double, Double), Double, Double]((xs zip ys).toList, (xy: (Double, Double)) => (xy._1 - mu(xs)) * (xy._2 - mu(ys)), (d: Double) => d).toArray) / (math.sqrt(variance(xs)) * math.sqrt(variance(ys)))
        }

        def vectorToArray(v: Vector[Double]): Array[Double] = {
                def loop(tempA: Array[Double], index: Integer): Array[Double] = {
                        if (index < v.length) {
                                loop(Array(v(index)) ++ tempA, index + 1)
                        }
                        else {
                                tempA
                        }
                }

                loop(Array[Double](), 0)
        }

}
