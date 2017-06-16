# phyqty

Scala library to model physical quantities respecting dimensional analysis

The objective is twofold
* group in a single class all physical quantities and their relationship (like speed is ratio of length by duration),
* perform at compilation time checks on dimensions (like impossibility to add a length and a speed)

## Examples

Defining a quantity

    >>> import com.github.bruneli.phyqty.Quantity._
    >>> import com.github.bruneli.phyqty.PhyUnit._
    >>> val position = 1.56(m)
    >>> val time = 2(min)
    >>> val newPosition = position + 76(cm) + 15(mm)
    >>> val newTime = time * 2
    >>> val distance = newPosition - position
    >>> val duration = newTime - time
    >>> val speed = distance / duration

Creating custom units

    >>> import com.github.bruneli.phyqty.Quantity._
    >>> import com.github.bruneli.phyqty.PhyUnit._
    >>> import com.github.bruneli.phyqty.DecimalMultiplier._
    >>> val kW = kilo(watt)
    >>> val kWh = kW * h
    >>> 1(kWh).in(J)

Working with collection of quantities (measurements)

    >>> import com.github.bruneli.phyqty.Quantity._
    >>> import com.github.bruneli.phyqty.PhyUnit._
    >>> import import com.github.bruneli.phyqty.Quantities
    >>> val measurementPeriod = 100(ms)
    >>> val positions = Quantities(2(m), 3.5(m), 5(m), 10(m))
    >>> val distances = positions.diff(1)
    >>> val speeds = distances.dropna / measurementPeriod
    >>> val meanSpeed = speeds.mean
    >>> val averageSpeed = (positions.last - positions.head) / (positions.length - 1) / measurementPeriod

## Usage

### SBT

Add the following dependency to your `build.sbt`

    libraryDependencies += "com.github.bruneli.phyqty" %% "phyqty" % "0.1"

### Maven

Add the following dependency to your `pom` file

    <dependency>
        <groupId>com.github.bruneli.phyqty</groupId>
        <artifactId>phyqty_2.11</artifactId>
        <version>0.1</version>
    </dependency>