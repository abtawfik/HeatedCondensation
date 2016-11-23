import scala.math._

object ReadProfile {

	class StateVar(file: String){
    	// File operations
    	val rawData     = scala.io.Source.fromFile(file) mkString

    	val dataDouble  =  for { l <- rawData.split(" ").map(_.trim)
									      		 	 if l != ""
   									         } yield l.toDouble

 

    	def getVar(data: Seq[Double], col: Int, scale: Double, offset: Double, missing: Double): Seq[Double] = {
     	 for { vv <- data.indices
     	       if vv % 4  == 0 && data != missing
      	     } yield (data(vv+col) * scale) + offset
    	}


			// Functions
      def columnSum   ( incoming: Seq[Double] ): Seq[Double] = incoming map{var s=0.0; d =>{s += d; s} }
			def midLevel( data: Seq[Double], pressure: Seq[Double] ) = {
          val levels  =  data.size - 1
      		for { ii <- data.indices if ii < levels } yield
      				(data(ii+1) * log(pressure(ii+1))  +  data(ii) * log(pressure(ii)))  /  log(pressure(ii+1)*pressure(ii))
			}
      def saturation_humidity( temp: Seq[Double], pressure: Seq[Double]) = {
          val epsilon  =   0.622
          val coeff0   =   6.11
          val A        =   17.269
          val B        =   35.86
          val t0       =   273.15
          val oneMep   =   1 - epsilon
          val t_and_p  =   temp zip pressure
          val qsat     =   t_and_p map { v => (epsilon   * (coeff0*exp((A*(v._1-t0)) / (v._1-B))) ) /
                                            (v._2-oneMep * (coeff0*exp((A*(v._1-t0)) / (v._1-B))))  }
          qsat map ( q => q/(1+q) )
      }


			// Operators
			implicit class PowerDouble(base: Double) {
    		def ^ (expo: Double): Double = pow(base, expo)
			}



  	  // State Variables
  	  val pressure  	=  getVar( dataDouble, 0, 1   , 0     , -9999 )
  	  val height   		=  getVar( dataDouble, 1, 1e-3, 0     , -9999 )
    	val spHumidity  =  getVar( dataDouble, 3, 1e-3, 0     , -9999 )
    	val tKelvin  		=  getVar( dataDouble, 2, 1   , 273.15, -9999 )
      val levels      =  tKelvin.size

      val dp     =  for ( ii <- pressure.indices if ii < levels-1 ) yield (pressure(ii) - pressure(ii+1))
      //      val theta  =  (tKelvin zip pressure) map { v => v._1 * ((1000/v._2)^0.286) }
      val theta  =  (tKelvin zip pressure) map { v => v._1 * pow(1000/v._2, 0.286) }
      val pressFromSurface  =  for ( ii <- pressure.indices ) yield pressure(0) - pressure(ii)

      val qmid  =  midLevel( spHumidity, pressure )
      val tmid  =  midLevel( tKelvin   , pressure )
      val pmid  =  midLevel( pressure  , pressure )

      val precipitableWater = columnSum( (qmid zip dp) map ( v  =>  v._1 * v._2 / 9.81 * 1e2 ) )
      val columnDensity     = columnSum(           dp  map ( dp =>           dp / 9.81 * 1e2 ) )
      val qmix 							= ( precipitableWater zip columnDensity ) map { v => v._1 / v._2 }
      val qsat 							= saturation_humidity( tKelvin, pressure )
      val qdef              = ( qsat zip qmix ) map ( v => v._1 - v._2 )


      def linearInterpolate ( x: Seq[Double], y:Seq[Double] ) = {
          val saturated    =  x filter (_ < 0 ) head
          val thisIndex    =  x indexWhere (_ == saturated )
          y(thisIndex) - ( (y(thisIndex)-y(thisIndex-1))/(x(thisIndex)-x(thisIndex-1)) * x(thisIndex) )
      }
      val pbcl  =  exp( linearInterpolate( qdef, pressure map (p => log(p)) ) )
      val tbm   =       linearInterpolate( qdef, theta )

  }





   val ARM = new StateVar("/Users/abtawfik/Desktop/TOPEKA.july.12.12Z.2016.csv")
                                                  //> java.io.FileNotFoundException: /Users/abtawfik/Desktop/TOPEKA.july.12.12Z.2
                                                  //| 016.csv (No such file or directory)
                                                  //| 	at java.io.FileInputStream.open(Native Method)
                                                  //| 	at java.io.FileInputStream.<init>(FileInputStream.java:131)
                                                  //| 	at scala.io.Source$.fromFile(Source.scala:91)
                                                  //| 	at scala.io.Source$.fromFile(Source.scala:76)
                                                  //| 	at scala.io.Source$.fromFile(Source.scala:54)
                                                  //| 	at ReadProfile$StateVar.<init>(ReadProfile.scala:7)
                                                  //| 	at ReadProfile$$anonfun$main$1.apply$mcV$sp(ReadProfile.scala:87)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$$anonfun$$exe
                                                  //| cute$1.apply$mcV$sp(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.scala:65)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$execute(Wor
                                                  //| ksheetSupport.scala:75)
                                                  //| 	at ReadProfile$.main(ReadProfile.scala:81)
                                                  //| 	at ReadProfile.main(ReadProfile.scala)

	 println( "  this is the column depth  =  " + ARM.levels )

   val WARMER = new StateVar("/Users/abtawfik/Desktop/TOPEKA.july.12.12Z.2016.warmer.prn")



    
   ARM.tbm
   WARMER.tbm


   ARM.pbcl
   WARMER.pbcl

   ARM.tbm/ARM.theta(0)
   WARMER.tbm/WARMER.theta(0)

   ARM.theta(0)
   WARMER.theta(0)






   

}