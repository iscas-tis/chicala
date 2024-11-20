# Chicala

Chicala is a **Chi**sel to S**cala** Translator.
It uses a Scala compiler plugin to translate Chisel hardware design code to
Scala simulation code.
This translation alows us to use software verification technology on the Scala
code to varify the Chisel hardware design, and use Chisel level information to
improve the verification, such as parameter and structure information.

We [verified arithmetic Chisel
designs](https://github.com/fengwz17/divider-stainless) through this translation
using [Stainless](https://github.com/epfl-lara/stainless).
Parameter information allows us to prove all different bit widths at once, and
structure information provides convenience for the proof.
For more details, see our paper in DAC24:  
*Formally Verifying Arithmetic Chisel Designs for All Bit Widths at Once*

## Usage

Publish this compiler plugin locally.

```shell
# in this project
sbt publishLocal
```

Add the compiler plugin to your Chisel program.
In your `build.sbt` add:

```scala
addCompilerPlugin("cn.ac.ios.tis" %% "chicala" % "0.1.0-SNAPSHOT")
```

Compile your Chisel program.
The translated Scala code for Stainless will be put in
`<your-chisel-project-dir>/test_run_dir/chiselToScala`.

```shell
# in your chisel project
sbt compile
```

Stainless version Scala codes are not able to run simulations due to some type
conversion issues.
We also support generating simulation-capable Scala codes by adding a
`scalacOptions` or compiling your Chisel project by:

```shell
# in your chisel project
sbt 'set scalacOptions+="-P:chicala:simulation:true"' compile
```

This tool is developed based on Chisel3.5.6 and only supports limited keywords
and structures.

Dependency libraries for translated code can be found in
[divider-stainless](https://github.com/fengwz17/divider-stainless)`/stain/src/main/scala/verify/libraryUInt`
for Stainless and
[chicala-soundness](https://github.com/liuyic00/chicala-soundness)`/src/main/scala/librarySimUInt`
for simulation.

## Examples

Run `make test` to see how this tool translates hardware designs in  `./testcase`.

For verification with Stainless, see
[our proof project](https://github.com/fengwz17/divider-stainless).

For simulation, see the [project](https://github.com/liuyic00/chicala-soundness)
where we compare the simulation results of our Scala code and
[ChiselTest](https://github.com/ucb-bar/chiseltest).

## Publications

**DAC 2024: Formally Verifying Arithmetic Chisel Designs for All Bit Widths at Once**

Efficient verification of ALUs has always been a challenge. Traditionally, they are verified at a low level, leading to state space explosion for larger bit widths. We symbolically can verify ALUs for all bit widths at once.
Chisel is a hardware description language embedded in Scala. Our key idea is to transform arithmetic Chisel designs into Scala software programs that simulate their behavior, then apply Stainless, a deductive formal verification tool for Scala. We validate the effectiveness by verifying dividers and multipliers in two open-source RISC-V processors, and conclude that our approach requires less manual guidance than others.

[ACM Digital Library](https://dl.acm.org/doi/10.1145/3649329.3657311) | [BibTex](https://dblp.org/rec/conf/dac/FengL0J0W24.html?view=bibtex&param=1)
