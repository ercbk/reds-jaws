Notes
================

-   [Calculations](#calculations)
    -   [Eligibility for the JAWS calculation](#eligibility-for-the-jaws-calculation)
    -   [Choosing Primary Positions](#choosing-primary-positions)
    -   [Weights](#weights)
    -   [Hall of Fame Score](#hall-of-fame-score)
-   [Issues](#issues)
    -   [Pitcher JAWS](#pitcher-jaws)
    -   [Names](#names)
    -   [Home](#home)
    -   [JAWS-4](#jaws-4)

Calculations
------------

### Eligibility for the JAWS calculation

The four-season-tenured constraint was decided upon by examining the distribution of the members of the Reds Hall of Fame. My initial group was only 81 players so I didn't want trim too much but I also wanted a final group that would provide a good measuring stick for future nominees. Sticking with seven years would be great. It would allow me to stay consistent with the original JAWS but it seemed unlikely. My initial thinking was that five years would satisfy my conditions.

I began by looking at common outlier measurements regarding the tenures of inductees and focused on values on the low end. There isn't a concern with players playing *too* long for JAWS calculation. Hopefully this would give me baseline from where I could start a decision process.

![\\text{threshold} = \\text{median} - 2\*\\text{MAD} = 3.0696](https://latex.codecogs.com/png.latex?%5Ctext%7Bthreshold%7D%20%3D%20%5Ctext%7Bmedian%7D%20-%202%2A%5Ctext%7BMAD%7D%20%3D%203.0696 "\text{threshold} = \text{median} - 2*\text{MAD} = 3.0696")

![\\text{threshold} = \\text{1st quartile} - 1.5\*\\text{IQR} = 1](https://latex.codecogs.com/png.latex?%5Ctext%7Bthreshold%7D%20%3D%20%5Ctext%7B1st%20quartile%7D%20-%201.5%2A%5Ctext%7BIQR%7D%20%3D%201 "\text{threshold} = \text{1st quartile} - 1.5*\text{IQR} = 1")

These values weren't very helpful to be honest. A one year floor wasn't worth considering. Three years seemed low, too. So these values didn't give me a desirable statistical floor.

Next I looked at the histogram and table of the different tenure values to see what percentage of players would remain at each cutoff. In my mind, I didn't want to go below 80%, but given that I'm starting with an intial group of 81 players, that was probably too low.

| cutoff | % remaining |
|:------:|:-----------:|
|  7 yrs |     67%     |
|  6 yrs |     78%     |
|  5 yrs |     86%     |
|  4 yrs |     92%     |

From here, the choice narrowed to four or five years.

Lastly, I looked at the nominees and it turns out Scott Rolen only played four seasons. The goal of this project is to evaluate nominees so four years would be the necessary cutoff in order for Rolen to be included. Yeah, I should've checked that out to begin with. I don't consider this subject closed though and plan on revisiting it in the future. Your opinions are welcome. So the inductees that didn't make it were the following: Billy Werber, Bill McKechnie, and Wayne Granger. The Wright boys, George and Harry, also aren't in there. They played with the Reds before 1871 and their WAR wasn't available.

### Choosing Primary Positions

Jaffe chooses primary positions according to the most WAR accumulated at a position. I had access to WAR per season but not per position and the information in the databases wasn't sufficient for me to perform the calculation. So I went with most games at a position during Reds tenure.

### Weights

Since there are different numbers of players enshrined in the HOF at each position, he evens out the position counts by adding "average" HOF players to each position (excluding pitcher) until the count at each position reaches the amount of the position that has the most players. From my understanding, he uses the mean of the WAR/JAWS values to calculate his average player used to fill each position. I'm using the median to be on the safe side.

### Hall of Fame Score

This score is just a standardization of the player stats. There are many standardization methods but I didn't want to wade to deep into the area. I looked at two: one uses mean and standard deviation and the other uses median and MAD.

![\\text{score} = \\frac{\\text{player stat value} - \\text{HOF stat median}}{\\text{HOF stat MAD}}](https://latex.codecogs.com/png.latex?%5Ctext%7Bscore%7D%20%3D%20%5Cfrac%7B%5Ctext%7Bplayer%20stat%20value%7D%20-%20%5Ctext%7BHOF%20stat%20median%7D%7D%7B%5Ctext%7BHOF%20stat%20MAD%7D%7D "\text{score} = \frac{\text{player stat value} - \text{HOF stat median}}{\text{HOF stat MAD}}")

![\\text{score} = \\frac{\\text{player stat value} - \\text{HOF stat mean}}{\\text{HOF stat sd}}](https://latex.codecogs.com/png.latex?%5Ctext%7Bscore%7D%20%3D%20%5Cfrac%7B%5Ctext%7Bplayer%20stat%20value%7D%20-%20%5Ctext%7BHOF%20stat%20mean%7D%7D%7B%5Ctext%7BHOF%20stat%20sd%7D%7D "\text{score} = \frac{\text{player stat value} - \text{HOF stat mean}}{\text{HOF stat sd}}")

I won't get into all the pros and cons. I'll just say I decided to go with the median/MAD method because of the skewness of many of the stat distributions. From briefly looking at both of the methods' results, I didn't find much difference. I tried to capture most of the variance between the two methods by setting the median in the scale (README.md) at -0.5 &lt; score &lt; 0.5.

Also, I flipped the sign from positive to negative for SO and K% for batting stats and for L, ERA, ERA-, FIP-, xFIP-, SIERA, WHIP, BB/9, HR/9, AVG, and BB% for pitching stats. This should create a consistency for user analysis. For every stat, a positive score can now be considered "good" and a negative score as "bad".

Issues
------

### Pitcher JAWS

Currently the WAR values in the openWARData package use a pre-2013 Baseball-Reference replacement player level. After randomly choosing some players, this change only seems to have affected Pitcher WAR. I've created an [issue](https://github.com/beanumber/openWARData/issues/11) and they seem to be looking into it.

While the WAR values won't match the values at the Baseball-Reference site, it shouldn't affect the efficacy of the project too much. There isn't any mixing of pre-2013 WAR with post-2013 WAR so player comparisons are still apples to apples.

### Names

I worked with Baseball-Reference and FanGraphs data and there were some name conflicts. Other sources agreed with Baseball-Reference for most if not all of the instances. Other incidents involved using nicknames or not. Baseball-Reference tended to use nicknames and I'd already used their names for the other conflicts so it was easier to just stick with them. Plus some of the nicknames were too cool not to use. For Griffey Jr., I decided to not use any punctuation.

-   Ken Griffey Jr not Ken Griffey ~~Jr.~~ or ~~JR~~
-   Dick Hoblitzell not Dick ~~Hoblitzel~~
-   Mike Smith not ~~Elmer~~ Smith
-   Ed Taubensee not ~~Eddie~~ Taubensee
-   Gene Thompson not ~~Junior~~ Thompson
-   High Pockets Kelly not ~~George~~ Kelly
-   Ice Box Chamberlain not ~~Elton~~ Chamberlain

### Home

On the home page of the dashboard, you might notice there isn't a distribution for third basemen. You might also notice that there are only two players with their primary position being third base.

### JAWS-4

Table: For pitchers, the values in the weighted columns aren't weighted. I didn't want to create a separate column for pitchers.

Cleveland Dot Plots: In the legend, the label "Typical HOFer" says the value is weighted. This is ***not*** the case for pitchers or groups such as CI, MI, OF, CO, or Md.

<table>
<caption>
Inductees
</caption>
<thead>
<tr>
<th style="text-align:left;">
Cutoff
</th>
<th style="text-align:left;">
% Cutoff
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
7 yrs
</td>
<td style="text-align:left;">
67%
</td>
</tr>
<tr>
<td style="text-align:left;">
6 yrs
</td>
<td style="text-align:left;">
78%
</td>
</tr>
<tr>
<td style="text-align:left;">
5 yrs
</td>
<td style="text-align:left;">
86%
</td>
</tr>
<tr>
<td style="text-align:left;">
4 yrs
</td>
<td style="text-align:left;">
92%
</td>
</tr>
</tbody>
</table>
<!-- %>% --> <!--      kable_styling() %>% --> <!--      row_spec(4, bold = T, color = "white", background = "#C6011F") -->
