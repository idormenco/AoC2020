let testInput =
    [| 35L
       20L
       15L
       25L
       47L
       40L
       62L
       55L
       65L
       95L
       102L
       117L
       150L
       182L
       127L
       219L
       299L
       277L
       309L
       576L |]

let problemData =
    [| 34L
       15L
       45L
       16L
       30L
       43L
       36L
       21L
       32L
       18L
       14L
       31L
       47L
       41L
       22L
       39L
       9L
       38L
       6L
       7L
       42L
       46L
       4L
       3L
       8L
       10L
       51L
       24L
       45L
       17L
       19L
       28L
       11L
       12L
       23L
       80L
       13L
       15L
       14L
       63L
       16L
       20L
       9L
       32L
       18L
       7L
       79L
       26L
       30L
       21L
       25L
       22L
       31L
       24L
       27L
       29L
       46L
       35L
       44L
       39L
       23L
       28L
       53L
       33L
       60L
       34L
       73L
       55L
       36L
       95L
       107L
       43L
       45L
       48L
       47L
       77L
       92L
       61L
       50L
       74L
       93L
       67L
       51L
       66L
       56L
       57L
       62L
       69L
       200L
       70L
       79L
       87L
       81L
       83L
       114L
       94L
       98L
       95L
       97L
       148L
       101L
       106L
       118L
       107L
       123L
       113L
       108L
       139L
       119L
       195L
       126L
       131L
       162L
       176L
       160L
       343L
       164L
       175L
       177L
       203L
       189L
       296L
       337L
       272L
       232L
       246L
       213L
       241L
       236L
       227L
       234L
       373L
       303L
       351L
       295L
       257L
       291L
       352L
       554L
       363L
       339L
       536L
       366L
       624L
       392L
       402L
       440L
       449L
       445L
       459L
       470L
       468L
       530L
       686L
       566L
       491L
       649L
       831L
       797L
       548L
       657L
       768L
       691L
       731L
       889L
       1234L
       758L
       1276L
       794L
       1459L
       1016L
       885L
       1704L
       904L
       927L
       938L
       959L
       1215L
       1727L
       2418L
       3186L
       1239L
       1205L
       1279L
       1306L
       1643L
       1616L
       1422L
       1552L
       1647L
       3038L
       1662L
       1698L
       2238L
       2543L
       1789L
       1812L
       1842L
       1865L
       2143L
       2606L
       4241L
       3263L
       2901L
       5792L
       3563L
       4563L
       4268L
       3069L
       6464L
       2974L
       3084L
       3120L
       3199L
       3309L
       5041L
       3360L
       3487L
       6571L
       3601L
       4962L
       3654L
       4743L
       4008L
       4749L
       5507L
       5985L
       10355L
       5875L
       6043L
       6628L
       6393L
       6058L
       6094L
       8109L
       6173L
       6204L
       8751L
       14152L
       6669L
       9472L
       7088L
       12117L
       12595L
       11136L
       7662L
       8397L
       18901L
       14845L
       10256L
       12079L
       11860L
       13062L
       11918L
       14790L
       12152L
       20257L
       12262L
       24262L
       32659L
       12842L
       12873L
       13757L
       14331L
       23098L
       14750L
       26668L
       16059L
       17918L
       18798L
       18653L
       23778L
       22116L
       22335L
       22174L
       44035L
       24012L
       25104L
       24070L
       25715L
       24414L
       47220L
       25135L
       28088L
       41194L
       26599L
       27204L
       32984L
       29081L
       30809L
       32668L
       33977L
       64786L
       43053L
       37451L
       40769L
       49549L
       46749L
       44509L
       51013L
       58383L
       53093L
       49785L
       67793L
       53223L
       51618L
       55944L
       62586L
       62065L
       56285L
       74746L
       83902L
       66532L
       59890L
       73862L
       66645L
       78220L
       81960L
       80504L
       120969L
       125028L
       91258L
       94294L
       95522L
       150238L
       202929L
       103008L
       101403L
       118350L
       104841L
       107562L
       112229L
       118871L
       157764L
       161293L
       126422L
       138110L
       126535L
       203084L
       140507L
       174207L
       171762L
       162464L
       174798L
       279643L
       213632L
       185552L
       189816L
       196925L
       204411L
       206244L
       221879L
       208965L
       248069L
       245348L
       219791L
       238764L
       245293L
       252957L
       264532L
       264645L
       419876L
       368687L
       315305L
       302971L
       357314L
       334226L
       337262L
       360350L
       389963L
       486524L
       547130L
       605643L
       401336L
       484057L
       484323L
       491721L
       780226L
       590219L
       458555L
       517602L
       498250L
       567616L
       517489L
       529177L
       888213L
       637197L
       618276L
       640233L
       660285L
       859891L
       671488L
       821585L
       1227416L
       791299L
       1135447L
       948466L
       987732L
       885393L
       956805L
       1096793L
       950276L
       1098788L
       1450110L
       976044L
       1015739L
       1027427L
       1157722L
       1046666L
       1200665L
       1255473L
       1503669L
       1687227L
       1300518L
       2003471L
       1651190L
       1462787L
       1612884L
       1964205L
       1676692L
       1905271L
       1833859L
       2704119L
       1835669L
       1991783L
       1926320L
       3799874L
       2022710L
       5633733L
       2043166L
       2062405L
       2074093L
       2302139L
       2247331L
       2501183L
       3856569L
       2763305L
       2913402L
       3075671L
       3113977L
       3139479L
       5136687L
       3448553L
       3510551L
       4014493L
       3897054L
       3669528L
       3761989L
       3827452L
       3918103L
       7566582L
       5930662L
       4803322L
       6419286L
       4806471L
       7118081L
       6027379L
       5916859L
       11132574L
       6562530L
       17695104L
       6211858L
       7090164L
       6189648L
       6253456L
       8647238L
       10080908L
       7180079L
       7911547L
       7431517L
       9678848L
       21213482L
       7680092L
       8724574L
       8721425L
       9609793L
       11983401L
       10720181L
       13643375L
       10723330L
       27776012L
       16327330L
       12106507L
       12401506L
       12443104L
       12465314L
       27403130L
       21662249L
       13369727L
       40772857L
       16404666L
       15901504L
       14611596L
       17289885L
       22829837L
       16401517L
       17445999L
       22094301L
       20827932L
       33229438L
       22826688L
       22703582L
       30933260L
       25749882L
       23124836L
       24508013L
       24549611L
       24571821L
       30815726L
       24908418L
       27076910L
       33191389L
       31016262L
       27981323L
       30513100L
       66420827L
       39231354L
       45954673L
       33691402L
       39526353L
       40149581L
       38273931L
       44920989L
       43531514L
       45530270L
       48874718L
       45828418L
       50201746L
       47632849L
       49057624L
       49079834L
       49121432L
       90875662L
       77038947L
       55058233L
       72902312L
       58494423L
       58997585L
       61672725L
       64204502L
       84761624L
       71965333L
       73217755L
       134575037L
       110752559L
       119598182L
       83194920L
       88452503L
       89061784L
       94587894L
       93461267L
       94908252L
       96754281L
       96690473L
       157979379L
       108077419L
       104179665L
       113552656L
       120670310L
       114055818L
       199814343L
       130459756L
       123202087L
       193241449L
       185816065L
       166679022L
       145183088L
       172256704L
       181913770L
       171647423L
       176656187L
       179949201L
       177514287L
       193444754L
       188049161L
       243872397L
       225368008L
       219956368L
       200870138L
       231279506L
       212257084L
       217732321L
       227608474L
       316443536L
       324072225L
       309018152L
       253661843L
       268385175L
       343335209L
       311862110L
       399865178L
       408005529L
       343904127L
       351596624L
       356605388L
       367998362L
       357463488L
       365563448L
       381493915L
       413127222L
       625737850L
       625384306L
       418602459L
       454531981L
       439865558L
       429989405L
       445340795L
       375054920L
       562679995L
       522047018L
       565523953L
       580247285L
       884521386L
       784165907L
       655766237L
       695500751L
       700509515L
       709467575L
       722168836L
       714068876L
       998849744L
       723026936L
       740618368L
       794621137L
       831729681L
       793657379L
       1162892494L
       1554756617L
       952036423L
       1229506702L
       805044325L
       1239961932L
       1647537174L
       1084727013L
       1102294303L
       1411976966L
       1236013522L
       1377935073L
       1351266988L
       1418527687L
       1423536451L
       1962130768L
       1431636411L
       1952533638L
       1437095812L
       1463645304L
       2702749136L
       1534275747L
       1588278516L
       1598701704L
       1745693802L
       1757080748L
       1889771338L
       2838663636L
       2320740535L
       1907338628L
       2855623499L
       3374107734L
       2782903399L
       2338307825L
       2774803439L
       2587280510L
       3340065841L
       3209339106L
       3344434440L
       3177330213L
       2868732223L
       4448442938L
       2900741116L
       3035797516L
       2997921051L
       4746002264L
       4121556257L
       3186980220L
       3355782452L
       3502774550L
       4612704247L
       3797109966L
       4659048360L
       4228079163L
       4245646453L
       5193931324L
       4925588335L
       5515638038L
       7635423158L
       5362083949L
       5774260730L
       9312748004L
       5898662167L
       5866653274L
       5769473339L
       5936538632L
       6256523568L
       7648501763L
       6033718567L
       6184901271L
       7932982484L
       6858557002L
       6542762672L
       12858570819L
       7748421003L
       10655666968L
       11092593491L
       14189506052L
       8473725616L
       9171234788L
       10119519659L
       11414300205L
       11260746116L
       11131557288L
       12441424839L
       13831644651L
       11636126613L
       11668135506L
       11706011971L
       11803191906L
       17197284748L
       12218619838L
       14507444183L
       12576481239L
       12727663943L
       13401319674L
       14291183675L
       15016488288L
       26765987291L
       25321063340L
       17644960404L
       21201389559L
       19290754447L
       20839370294L
       20302792076L
       21251076947L
       23988410059L
       22966758087L
       22767683901L
       23304262119L
       24395799449L
       36935714851L
       27083925422L
       27018847618L
       24021811744L
       31867235686L
       26128983617L
       25304145182L
       25977800913L
       27692503349L
       46817171207L
       46968353911L
       35855858582L
       38896037351L
       48220237177L
       45268555360L
       39593546523L
       40130124741L
       44217835034L
       44018760848L
       44555339066L
       67723094268L
       46988569831L
       46071946020L
       47326073863L
       48417611193L
       49325956926L
       51281946095L
       50150795361L
       49999612657L
       51433128799L
       53821486966L
       53670304262L
       100658874093L
       88895650008L
       81927804602L
       74751895933L
       93951611707L
       85884607182L
       84347959775L
       84148885589L
       79723671264L
       88773174100L
       100810056797L
       94018373505L
       147688677767L
       152092002892L
       124902691294L
       172228765157L
       144102407068L
       101432741456L
       99325569583L
       101281558752L
       100150408018L
       131927417259L
       105103433061L
       222440573700L
       137819189851L
       154475567197L
       159099855708L
       178100497296L
       158900781522L
       181005230016L
       260381414460L
       163872556853L
       168496845364L
       341973054149L
       291791084835L
       242922622912L
       193343943088L
       199475977601L
       200607128335L
       337001278818L
       321766143283L
       200758311039L
       201431966770L
       204429002644L
       205253841079L
       232077825277L
       491476846015L
       259579000258L
       515101776114L
       460988542795L
       313376348719L
       318000637230L
       322773338375L
       809477483245L
       332369402217L
       523531649414L
       364630867892L
       361840788452L
       398597784167L
       392819920689L
       517805351363L
       518758948269L
       402039095105L
       720190915039L
       406685807849L
       636149687094L
       405187313683L
       405860969414L
       437331666356L
       607292936184L
       491656825535L
       1015188474949L
       572955348977L
       822829331247L
       965775269666L
       645745750936L
       729459146224L
       655142740592L
       1237996266402L
       694210190669L
       1159681336508L
       835929450523L
       754660709141L
       1272466060504L
       794859015794L
       956090614625L
       1361951584039L
       807226408788L
       811048283097L
       898342633384L
       1012480249867L
       842518980039L
       1137402576471L
       928988491891L
       1064612174512L
       1389999458919L
       1218701099913L
       1228098089569L
       1300888491528L
       1650300805294L
       1339955941605L
       1349352931261L
       1409803449733L
       1489069206463L
       1448870899810L
       1995610787031L
       1823528532964L
       1549519724935L
       1705569042172L
       2169177992827L
       1618274691885L
       1771507471930L
       1649745388827L
       1653567263136L
       1740861613423L
       1993600666403L
       1907131154551L
       2157086581460L
       3090214544684L
       2682886866397L
       2528986581097L
       2446799189482L
       2568054031174L
       2958230633490L
       2689308872866L
       4076309147378L
       3396200361014L
       4795783586856L
       4117573756109L
       4391582564138L
       3167794416820L
       3199265113762L
       3203086988071L
       5137061974437L
       3425074735066L
       4852832376898L
       3303312651963L
       3390607002250L
       3394428876559L
       3900731820954L
       4522587247500L
       5696780997917L
       4603885770942L
       6697741528522L
       4975785770579L
       5014853220656L
       5136108062348L
       7803150884704L
       6079915875116L
       5857103289686L
       8318165872619L
       6367059530582L
       7721852361262L
       6370881404891L
       6402352101833L
       6471107068783L
       6785035878809L
       6506399640034L
       6693919654213L
       7291338823204L
       10598473349476L
       11607215131131L
       7295160697513L
       7917016124059L
       8423319068454L
       14092733766153L
       11346667175470L
       14284075654641L
       9990638991235L
       10111893832927L
       10150961283004L
       17793789875939L
       16618293472961L
       11937019164802L
       16478953363509L
       12737940935473L
       13155917283700L
       14388123192842L
       19209048004256L
       13096271756046L
       12977506708817L
       24487709530152L
       14610935778272L
       15117238722667L
       14586499520717L
       24324173884287L
       22412399420180L
       15212176821572L
       16340335192513L
       18413958059689L
       24577138511952L
       23089400541744L
       22087980447806L
       20102532824162L
       22849834768400L
       22888902218477L
       24674960100275L
       27564006229534L
       24914525873619L
       25715447644290L
       25834212691519L
       26073778464863L
       28094745431484L
       38301577363316L
       27588442487089L
       28189683530389L
       36442868016675L
       29197435298989L
       42776183051106L
       29798676342289L
       37624576241752L
       45817980468452L
       33626134881261L
       34754293252202L
       47611393358678L
       42190513271968L
       48565282412690L
       42952367592562L
       53422655178608L
       45738736986877L
       52687578560766L
       49589485973894L
       50988304338482L
       51789226109153L
       51549660335809L
       88008493740420L
       79083049769966L
       72574859393395L
       55778126017478L
       56785877786078L
       106110233739374L |]

let isSumOfPreambleNumbers preamble number =
    let rec loop preamble number =
        match preamble with
        | [] -> false
        | h :: t ->
            (preamble
             |> List.except [ h ]
             |> List.contains (number - h))
            || (preamble
                |> List.except [ h ]
                |> List.contains (h - number))
            || (loop t number)

    loop preamble number

let checkSequence seq preambleSize =
    let numbersToCheck = seq |> List.skip preambleSize
    let preamble = seq |> List.take preambleSize

    let rec loop preamble numbers =
        match numbers with
        | [] -> failwith ("did not treat this case")
        | h :: t when (isSumOfPreambleNumbers preamble h) -> loop ((preamble |> List.skip 1) @ [ h ]) t
        | h :: _ -> h

    loop preamble numbersToCheck

let rec sumWhile seq sum acc =
    match seq with
    | [] -> None
    | h :: t when ((acc |> List.sum) + h) = sum -> Some(h :: acc)
    | h :: t when ((acc |> List.sum) + h) > sum -> None
    | h :: t -> sumWhile t sum (h :: acc)

let getSequenceForSum seq sum =
    let rec loop seq sum =
        match seq with
        | [] -> []
        | h :: t ->
            match (sumWhile seq sum []) with
            | None -> loop t sum
            | Some lst -> lst

    loop seq sum

let result =
    checkSequence (problemData |> Seq.toList) 25

let encryptionWeakness =
    getSequenceForSum (problemData |> Seq.toList) 375054920L

let min = encryptionWeakness |> List.min
let max = encryptionWeakness |> List.max
let result2 = min + max
