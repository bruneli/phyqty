# phyqty

Scala library to model physical quantities respecting dimensional analysis

The objective is twofold
* group in a single class all physical quantities and their relationship (like speed is ratio of length by duration),
* perform at compilation time checks on physical dimensions (like impossibility to add a length and a speed)

## Examples

Defining a scalar or vector quantity

    >>> import com.github.bruneli.phyqty.ScalarQuantity._
    >>> import com.github.bruneli.phyqty.VectorQuantity._
    >>> import com.github.bruneli.phyqty.PhyUnit._
    >>> val x0 = vector(1.56(m), 2.5(cm))
    >>> val t0 = 2(min) // scalar quantity
    >>> val x1 = vector(20.8(m), -39.5(m))
    >>> val t1 = t0 * 2
    >>> val dx = x1 - x0
    >>> dx.magnitude // distance
    >>> val dt = t1 - t0
    >>> val v = dx / dt
    >>> val m = 50(kg)
    >>> val Ekin = m * v * v / 2

Creating custom units

    >>> import com.github.bruneli.phyqty.ScalarQuantity._
    >>> import com.github.bruneli.phyqty.PhyUnit._
    >>> import com.github.bruneli.phyqty.DecimalMultiplier._
    >>> val kW = kilo(watt)
    >>> val kWh = kW * h
    >>> 1(kWh).in(J)

Working with collection of scalar or vector quantities (measurements)

    >>> import com.github.bruneli.phyqty.ScalarQuantity._
    >>> import com.github.bruneli.phyqty.PhyUnit._
    >>> import com.github.bruneli.phyqty.ScalarQuantities
    >>> import com.github.bruneli.phyqty.VectorQuantities
    >>> val dt = 100(ms)
    >>> val x = ScalarQuantities(2(m), 3.5(m), 5(m), 10(m))
    >>> val dx = x.diff(1)
    >>> val y = ScalarQuantities(4(m), -2.5(m), -50(m), -25(m))
    >>> val positions = VectorQuantities(x, y)
    >>> val v = positions.diff(1).dropna / dt
    >>> val meanSpeed = v.mean.magnitude
    >>> val averageSpeed = (positions.last - positions.head) / (positions.length - 1) / dt

## Usage

### SBT

Add the following dependency to your `build.sbt`

    libraryDependencies += "com.github.bruneli.phyqty" %% "phyqty" % "1.0"

### Maven

Add the following dependency to your `pom` file

    <dependency>
        <groupId>com.github.bruneli.phyqty</groupId>
        <artifactId>phyqty_2.11</artifactId>
        <version>1.0</version>
    </dependency>