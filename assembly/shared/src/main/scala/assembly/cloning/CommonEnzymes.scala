package assembly.cloning

object CommonEnzymes {
  val common = Map[String, String](
    "SinI" -> "GGWCC",
    "BamHI" -> "GGATCC",
    "BmeRI" -> "GACNNNNNGTC",
    "SgrAI" -> "CRCCGGYG",
    "SphI" -> "GCATGC",
    "BspEI" -> "TCCGGA",
    "Tth111I" -> "GACNNNGTC",
    "Ecl35734I" -> "GAAAYTC",
    "BsiHKCI" -> "CYCGRG",
    "Sfr303I" -> "CCGCGG",
    "BspFNI" -> "CGCG",
    "CjuI" -> "CAYNNNNNRTG",
    "Lmo370I" -> "AGCGCCG",
    "BseLI" -> "CCNNNNNNNGG",
    "Bse118I" -> "RCCGGY",
    "Vtu19109I" -> "CACRAYC",
    "BsiSI" -> "CCGG",
    "AasI" -> "GACNNNNNNGTC",
    "HpyUM032XIV" -> "GAAAG",
    "EcoO109I" -> "RGGNCCY",
    "HpyF3I" -> "CTNAG",
    "SetI" -> "ASST",
    "SfiI" -> "GGCCNNNNNGGCC",
    "BlpI" -> "GCTNAGC",
    "HpyCH4IV" -> "ACGT",
    "Cba13II" -> "AGGAAT",
    "AcvI" -> "CACGTG",
    "PflMI" -> "CCANNNNNTGG",
    "CjeFV" -> "GGRCA",
    "StuI" -> "AGGCCT",
    "BstFNI" -> "CGCG",
    "BsoBI" -> "CYCGRG",
    "PspOMI" -> "GGGCCC",
    "BsaI" -> "GGTCTC",
    "MbiI" -> "CCGCTC",
    "CjeNIII" -> "GKAAYG",
    "DraII" -> "RGGNCCY",
    "BcnI" -> "CCSGG",
    "Aor51HI" -> "AGCGCT",
    "Pdu1735I" -> "CACCAC",
    "HindIII" -> "AAGCTT",
    "BscGI" -> "CCCGT",
    "AfeI" -> "AGCGCT",
    "FatI" -> "CATG",
    "BshTI" -> "ACCGGT",
    "BssNAI" -> "GTATAC",
    "HpyAXVI_mut1" -> "CRTTAA",
    "BstBI" -> "TTCGAA",
    "CalB3II" -> "GRTTRAG",
    "PsyI" -> "GACNNNGTC",
    "Pfl1108I" -> "TCGTAG",
    "BspMI" -> "ACCTGC",
    "Ssp714II" -> "CGCAGCG",
    "BfoI" -> "RGCGCY",
    "BseXI" -> "GCAGC",
    "AjiI" -> "CACGTC",
    "Esp3007I" -> "CAGAAG",
    "KspI" -> "CCGCGG",
    "AhaIII" -> "TTTAAA",
    "AlwNI" -> "CAGNNNCTG",
    "Bsp1720I" -> "GCTNAGC",
    "NgoAVIII" -> "GACNNNNNTGA",
    "BstPAI" -> "GACNNNNGTC",
    "RsaI" -> "GTAC",
    "FinI" -> "GGGAC",
    "CfrI" -> "YGGCCR",
    "PsiI" -> "TTATAA",
    "RdeGBIII" -> "TGRYCA",
    "DseDI" -> "GACNNNNNNGTC",
    "PleI" -> "GAGTC",
    "FaqI" -> "GGGAC",
    "Bag18758I" -> "CCCGAG",
    "BstZI" -> "CGGCCG",
    "BthCI" -> "GCNGC",
    "BseBI" -> "CCWGG",
    "MaeI" -> "CTAG",
    "Hpy99XIV" -> "GGWTAA",
    "BshFI" -> "GGCC",
    "NhaXI" -> "CAAGRAG",
    "BstMWI" -> "GCNNNNNNNGC",
    "BstH2I" -> "RGCGCY",
    "AvaII" -> "GGWCC",
    "MauBI" -> "CGCGCGCG",
    "RruI" -> "TCGCGA",
    "BoxI" -> "GACNNNNGTC",
    "TspEI" -> "AATT",
    "Sau96I" -> "GGNCC",
    "SseBI" -> "AGGCCT",
    "FspI" -> "TGCGCA",
    "AccIII" -> "TCCGGA",
    "Acc65I" -> "GGTACC",
    "Eco88I" -> "CYCGRG",
    "Asu14238IV" -> "CGTRAC",
    "BshVI" -> "ATCGAT",
    "RceI" -> "CATCGAC",
    "Bsp120I" -> "GGGCCC",
    "XmaI" -> "CCCGGG",
    "AdeI" -> "CACNNNGTG",
    "AvaI" -> "CYCGRG",
    "Hin1II" -> "CATG",
    "PpiI" -> "GAACNNNNNCTC",
    "SmoI" -> "CTYRAG",
    "BseRI" -> "GAGGAG",
    "Cfr9I" -> "CCCGGG",
    "Asp718I" -> "GGTACC",
    "AleI" -> "CACNNNNGTG",
    "BtsIMutI" -> "CAGTG",
    "Bsp1407I" -> "TGTACA",
    "NdeI" -> "CATATG",
    "TspMI" -> "CCCGGG",
    "SacI" -> "GAGCTC",
    "OliI" -> "CACNNNNGTG",
    "MnlI" -> "CCTC",
    "FspBI" -> "CTAG",
    "SecI" -> "CCNNGG",
    "EcoT22I" -> "ATGCAT",
    "SpnRII" -> "TCGAG",
    "SplI" -> "CGTACG",
    "HpyCH4V" -> "TGCA",
    "PteI" -> "GCGCGC",
    "AjnI" -> "CCWGG",
    "BveI" -> "ACCTGC",
    "PinAI" -> "ACCGGT",
    "Bse1I" -> "ACTGG",
    "BsuRI" -> "GGCC",
    "RsrII" -> "CGGWCCG",
    "EarI" -> "CTCTTC",
    "Bce83I" -> "CTTGAG",
    "HapII" -> "CCGG",
    "BcoDI" -> "GTCTC",
    "NaeI" -> "GCCGGC",
    "DinI" -> "GGCGCC",
    "AciI" -> "CCGC",
    "Hpy188III" -> "TCNNGA",
    "Cfr42I" -> "CCGCGG",
    "MfeI" -> "CAATTG",
    "SelI" -> "CGCG",
    "Eco24I" -> "GRGCYC",
    "SchI" -> "GAGTC",
    "UnbI" -> "GGNCC",
    "SgsI" -> "GGCGCGCC",
    "SnaI" -> "GTATAC",
    "UbaF12I" -> "CTACNNNGTC",
    "BplI" -> "GAGNNNNNCTC",
    "XmaIII" -> "CGGCCG",
    "BstACI" -> "GRCGYC",
    "Pdi8503III" -> "CCGGNAG",
    "SspDI" -> "GGCGCC",
    "AvrII" -> "CCTAGG",
    "SsiI" -> "CCGC",
    "NlaIII" -> "CATG",
    "SgfI" -> "GCGATCGC",
    "BfuI" -> "GTATCC",
    "Alw26I" -> "GTCTC",
    "BciT130I" -> "CCWGG",
    "CspCI" -> "CAANNNNNGTGG",
    "EspI" -> "GCTNAGC",
    "AccBSI" -> "CCGCTC",
    "BsrI" -> "ACTGG",
    "Nal45188II" -> "ACCAGC",
    "EagI" -> "CGGCCG",
    "HaeI" -> "WGGCCW",
    "BssMI" -> "GATC",
    "BsbI" -> "CAACAC",
    "Jma19592I" -> "GTATNAC",
    "EaeI" -> "YGGCCR",
    "AluBI" -> "AGCT",
    "Bpu14I" -> "TTCGAA",
    "Eam1105I" -> "GACNNNNNGTC",
    "NruI" -> "TCGCGA",
    "BstPI" -> "GGTNACC",
    "PmeI" -> "GTTTAAAC",
    "BstAPI" -> "GCANNNNNTGC",
    "Cma23826I" -> "CGGAAG",
    "NlaIV" -> "GGNNCC",
    "Bsp1286I" -> "GDGCHC",
    "Hpy166II" -> "GTNNAC",
    "AccB1I" -> "GGYRCC",
    "Bso31I" -> "GGTCTC",
    "CchIII" -> "CCCAAG",
    "BspT107I" -> "GGYRCC",
    "AsuC2I" -> "CCSGG",
    "CseI" -> "GACGC",
    "Bst2BI" -> "CACGAG",
    "NsbI" -> "TGCGCA",
    "KpnI" -> "GGTACC",
    "EsaSSI" -> "GACCAC",
    "DsaI" -> "CCRYGG",
    "AcoI" -> "YGGCCR",
    "PaeI" -> "GCATGC",
    "HpyAXIV" -> "GCGTA",
    "FokI" -> "GGATG",
    "MaeII" -> "ACGT",
    "Bsu15I" -> "ATCGAT",
    "Kzo9I" -> "GATC",
    "PvuI" -> "CGATCG",
    "BcuI" -> "ACTAGT",
    "GsuI" -> "CTGGAG",
    "Rba2021I" -> "CACGAGH",
    "Ksp22I" -> "TGATCA",
    "BspMII" -> "TCCGGA",
    "CstMI" -> "AAGGAG",
    "PceI" -> "AGGCCT",
    "PauI" -> "GCGCGC",
    "ApeKI" -> "GCWGC",
    "Eco72I" -> "CACGTG",
    "ErhI" -> "CCWWGG",
    "EcoRII" -> "CCWGG",
    "BarI" -> "GAAGNNNNNNTAC",
    "FaeI" -> "CATG",
    "BmgI" -> "GKGCCC",
    "NspBII" -> "CMGCKG",
    "RpaB5I" -> "CGRGGAC",
    "BstV1I" -> "GCAGC",
    "PspPPI" -> "RGGWCCY",
    "Sth20745III" -> "GGACGAC",
    "Bbv12I" -> "GWGCWC",
    "PspGI" -> "CCWGG",
    "VneI" -> "GTGCAC",
    "MluNI" -> "TGGCCA",
    "HpyUM032XIII_mut1" -> "CYANNNNNNNTTC",
    "BsiEI" -> "CGRYCG",
    "BinI" -> "GGATC",
    "MslI" -> "CAYNNNNRTG",
    "XhoII" -> "RGATCY",
    "HpyUM032XIII" -> "CYANNNNNNNTRG",
    "HgiCI" -> "GGYRCC",
    "ApaLI" -> "GTGCAC",
    "Sse232I" -> "CGCCGGCG",
    "BsaHI" -> "GRCGYC",
    "Eco31I" -> "GGTCTC",
    "Sen13311III" -> "GATCAG",
    "Pst14472I" -> "CNYACAC",
    "Sse9I" -> "AATT",
    "AspDUT2V" -> "GNGCAAC",
    "PsrI" -> "GAACNNNNNNTAC",
    "MunI" -> "CAATTG",
    "AhlI" -> "ACTAGT",
    "UcoMSI" -> "GAGCTC",
    "Bsp143I" -> "GATC",
    "HaeIII" -> "GGCC",
    "SapI" -> "GCTCTTC",
    "ScaI" -> "AGTACT",
    "RpaTI" -> "GRTGGAG",
    "BsrBI" -> "CCGCTC",
    "BceAI" -> "ACGGC",
    "Bse8I" -> "GATNNNNATC",
    "MspR9I" -> "CCNGG",
    "Asp700I" -> "GAANNNNTTC",
    "MlyI" -> "GAGTC",
    "RlaI" -> "VCW",
    "SalI" -> "GTCGAC",
    "SwaI" -> "ATTTAAAT",
    "BseSI" -> "GKGCMC",
    "KpnNIH30III" -> "GTTCNAC",
    "AgsI" -> "TTSAA",
    "Hin4I" -> "GAYNNNNNVTC",
    "Psp10HII" -> "GRAGCAG",
    "Bsa29I" -> "ATCGAT",
    "EclXI" -> "CGGCCG",
    "SdeAI" -> "CAGRAG",
    "FriOI" -> "GRGCYC",
    "FbaI" -> "TGATCA",
    "BsiI" -> "CACGAG",
    "BbvII" -> "GAAGAC",
    "Bpu10I" -> "CCTNAGC",
    "OspHL35III" -> "YAGGAG",
    "AbsI" -> "CCTCGAGG",
    "BspHI" -> "TCATGA",
    "Hpy99XIII" -> "GCCTA",
    "BstC8I" -> "GCNNGC",
    "UbaF11I" -> "TCGTA",
    "Hsp92II" -> "CATG",
    "Ppu10I" -> "ATGCAT",
    "BspLI" -> "GGNNCC",
    "BsmFI" -> "GGGAC",
    "AlwI" -> "GGATC",
    "HhaI" -> "GCGC",
    "BfiI" -> "ACTGGG",
    "XmnI" -> "GAANNNNTTC",
    "Bsu36I" -> "CCTNAGG",
    "SnaBI" -> "TACGTA",
    "AspS9I" -> "GGNCC",
    "Gba708II" -> "ATGCAC",
    "Sth132I" -> "CCCG",
    "BsrGI" -> "TGTACA",
    "NgoAVII" -> "GCCGC",
    "NmeDI" -> "RCCGGY",
    "BstZ17I" -> "GTATAC",
    "BspD6I" -> "GACTC",
    "Cfr13I" -> "GGNCC",
    "AbaUMB2I" -> "YCCGSS",
    "Eco105I" -> "TACGTA",
    "Mph1103I" -> "ATGCAT",
    "BfuCI" -> "GATC",
    "PenI" -> "GCAGT",
    "BseNI" -> "ACTGG",
    "AquIII" -> "GAGGAG",
    "Saf8902III" -> "CAATNAG",
    "Csp2014I" -> "GGAGGC",
    "BglI" -> "GCCNNNNNGGC",
    "BmeDI" -> "C",
    "Ksp632I" -> "CTCTTC",
    "CcrNAIII" -> "CGACCAG",
    "TspGWI" -> "ACGGA",
    "HpyCH4III" -> "ACNGT",
    "PciSI" -> "GCTCTTC",
    "Psp03I" -> "GGWCC",
    "BstSLI" -> "GKGCMC",
    "VpaK11BI" -> "GGWCC",
    "BspNCI" -> "CCAGA",
    "Hin6I" -> "GCGC",
    "AluI" -> "AGCT",
    "BpmI" -> "CTGGAG",
    "HgiEII" -> "ACCNNNNNNGGT",
    "TfiI" -> "GAWTC",
    "ArsI" -> "GACNNNNNNTTYG",
    "NhoI" -> "GCWGC",
    "XapI" -> "RAATTY",
    "UbaPI" -> "CGAACG",
    "AjuI" -> "GAANNNNNNNTTGG",
    "McaTI" -> "GCGCGC",
    "SfoI" -> "GGCGCC",
    "MroI" -> "TCCGGA",
    "TasI" -> "AATT",
    "BsuI" -> "GTATCC",
    "Eco47I" -> "GGWCC",
    "Sth302II" -> "CCGG",
    "BtgI" -> "CCRYGG",
    "Ssp6803IV" -> "GAAGGC",
    "NspV" -> "TTCGAA",
    "NotI" -> "GCGGCCGC",
    "SgrBI" -> "CCGCGG",
    "Asi256I" -> "GATC",
    "SlaI" -> "CTCGAG",
    "BpuMI" -> "CCSGG",
    "Hpy99I" -> "CGWCG",
    "Kpn2I" -> "TCCGGA",
    "Csp6I" -> "GTAC",
    "BbvI" -> "GCAGC",
    "EcoRI" -> "GAATTC",
    "TsuI" -> "GCGAC",
    "Fnu4HI" -> "GCNGC",
    "Rsr2I" -> "CGGWCCG",
    "ApaI" -> "GGGCCC",
    "Esp3I" -> "CGTCTC",
    "BstSNI" -> "TACGTA",
    "EcoICRI" -> "GAGCTC",
    "Nli3877I" -> "CYCGRG",
    "RigI" -> "GGCCGGCC",
    "HpyF10VI" -> "GCNNNNNNNGC",
    "SfuI" -> "TTCGAA",
    "PdiI" -> "GCCGGC",
    "BfuAI" -> "ACCTGC",
    "SfeI" -> "CTRYAG",
    "PspOMII" -> "CGCCCAR",
    "Vha464I" -> "CTTAAG",
    "MluCI" -> "AATT",
    "AquII" -> "GCCGNAC",
    "MwoI" -> "GCNNNNNNNGC",
    "BsnI" -> "GGCC",
    "Fsp4HI" -> "GCNGC",
    "PspEI" -> "GGTNACC",
    "BstDEI" -> "CTNAG",
    "BspT104I" -> "TTCGAA",
    "MscI" -> "TGGCCA",
    "PfeI" -> "GAWTC",
    "CjeNII" -> "GAGNNNNNGT",
    "Bce3081I" -> "TAGGAG",
    "TaqII" -> "GACCGA",
    "CjuII" -> "CAYNNNNNCTC",
    "PsuI" -> "RGATCY",
    "BspGI" -> "CTGGAC",
    "XmaJI" -> "CCTAGG",
    "AclWI" -> "GGATC",
    "BmtI" -> "GCTAGC",
    "HgiJII" -> "GRGCYC",
    "BsmAI" -> "GTCTC",
    "MkaDII" -> "GAGAYGT",
    "Eco81I" -> "CCTNAGG",
    "BspOI" -> "GCTAGC",
    "Bsp19I" -> "CCATGG",
    "PabI" -> "GTAC",
    "Bse3DI" -> "GCAATG",
    "XspI" -> "CTAG",
    "AccII" -> "CGCG",
    "BmuI" -> "ACTGGG",
    "Hpy178III" -> "TCNNGA",
    "PagI" -> "TCATGA",
    "Bme18I" -> "GGWCC",
    "FauNDI" -> "CATATG",
    "Ama87I" -> "CYCGRG",
    "RgaI" -> "GCGATCGC",
    "CviJI" -> "RGCY",
    "Alw21I" -> "GWGCWC",
    "Cfr10I" -> "RCCGGY",
    "PmlI" -> "CACGTG",
    "BsiYI" -> "CCNNNNNNNGG",
    "XcmI" -> "CCANNNNNNNNNTGG",
    "RflFIII" -> "CGCCAG",
    "Awo1030IV" -> "GCCRAG",
    "Acc36I" -> "ACCTGC",
    "Tru9I" -> "TTAA",
    "DrdII" -> "GAACCA",
    "EcoE1140I" -> "ACCYAC",
    "Cgl13032I" -> "GGCGCA",
    "AlfI" -> "GCANNNNNNTGC",
    "HpyAXVI_mut2" -> "CRTCNA",
    "Bsp13I" -> "TCCGGA",
    "AseI" -> "ATTAAT",
    "BseCI" -> "ATCGAT",
    "DraIII" -> "CACNNNGTG",
    "TsoI" -> "TARCCA",
    "BseYI" -> "CCCAGC",
    "UbaF9I" -> "TACNNNNNRTGT",
    "NlaCI" -> "CATCAC",
    "BcefI" -> "ACGGC",
    "GauT27I" -> "CGCGCAGG",
    "CjeFIII" -> "GCAAGG",
    "XceI" -> "RCATGY",
    "SduI" -> "GDGCHC",
    "SdaI" -> "CCTGCAGG",
    "BanI" -> "GGYRCC",
    "TseI" -> "GCWGC",
    "Bsu7003I" -> "GACGAGC",
    "TaaI" -> "ACNGT",
    "AvaIII" -> "ATGCAT",
    "BslFI" -> "GGGAC",
    "BspDI" -> "ATCGAT",
    "SbfI" -> "CCTGCAGG",
    "PspPI" -> "GGNCC",
    "AspA2I" -> "CCTAGG",
    "HbaII" -> "GCCCAG",
    "Pal408I" -> "CCRTGAG",
    "BspACI" -> "CCGC",
    "Tsp45I" -> "GTSAC",
    "XagI" -> "CCTNNNNNAGG",
    "Cac8I" -> "GCNNGC",
    "BtrI" -> "CACGTC",
    "Bme1390I" -> "CCNGG",
    "HinP1I" -> "GCGC",
    "Pse18267I" -> "RCCGAAG",
    "ApaBI" -> "GCANNNNNTGC",
    "EsaBC3I" -> "TCGA",
    "SmaI" -> "CCCGGG",
    "GsaI" -> "CCCAGC",
    "MaqI" -> "CRTTGAC",
    "BspCNI" -> "CTCAG",
    "Lsp6406VI" -> "CRAGCAC",
    "RpaBI" -> "CCCGCAG",
    "AsuNHI" -> "GCTAGC",
    "AsuI" -> "GGNCC",
    "BsmI" -> "GAATGC",
    "EcoMII" -> "CANCATC",
    "BscAI" -> "GCATC",
    "SfaNI" -> "GCATC",
    "PscI" -> "ACATGT",
    "AclI" -> "AACGTT",
    "LguI" -> "GCTCTTC",
    "NmuCI" -> "GTSAC",
    "SpeI" -> "ACTAGT",
    "HincII" -> "GTYRAC",
    "SciI" -> "CTCGAG",
    "MspI7II" -> "ACGRAG",
    "BsePI" -> "GCGCGC",
    "Hpy99XIV_mut1" -> "GGWCNA",
    "BstUI" -> "CGCG",
    "AccI" -> "GTMKAC",
    "RpaI" -> "GTYGGAG",
    "StyI" -> "CCWWGG",
    "EcoHI" -> "CCSGG",
    "Psp0357II" -> "GCGAAG",
    "PspPRI" -> "CCYCAG",
    "BspQI" -> "GCTCTTC",
    "Cgl13032II" -> "ACGABGG",
    "BmgBI" -> "CACGTC",
    "MluI" -> "ACGCGT",
    "DraI" -> "TTTAAA",
    "Mva1269I" -> "GAATGC",
    "BmrFI" -> "CCNGG",
    "NciI" -> "CCSGG",
    "SatI" -> "GCNGC",
    "AfiI" -> "CCNNNNNNNGG",
    "EcoT14I" -> "CCWWGG",
    "BbsI" -> "GAAGAC",
    "HgaI" -> "GACGC",
    "BstDSI" -> "CCRYGG",
    "BsaXI" -> "ACNNNNNCTCC",
    "Lsp48III" -> "AGCACC",
    "Mba11I" -> "AGGCGA",
    "MtuHN878II" -> "CACGCAG",
    "StsI" -> "GGATG",
    "PssI" -> "RGGNCCY",
    "Tsp4CI" -> "ACNGT",
    "SmiMI" -> "CAYNNNNRTG",
    "CaiI" -> "CAGNNNCTG",
    "CsiI" -> "ACCWGGT",
    "BseJI" -> "GATNNNNATC",
    "Bsp24I" -> "GACNNNNNNTGG",
    "Alw44I" -> "GTGCAC",
    "Hin1I" -> "GRCGYC",
    "KroI" -> "GCCGGC",
    "Ecl136II" -> "GAGCTC",
    "FnuDII" -> "CGCG",
    "AsuII" -> "TTCGAA",
    "RdeGBII" -> "ACCCAG",
    "Pme5II" -> "GACGAG",
    "Sty13348III" -> "GATCAG",
    "BstBAI" -> "YACGTR",
    "HauII" -> "TGGCCA",
    "BssAI" -> "RCCGGY",
    "DpnII" -> "GATC",
    "BsrFI" -> "RCCGGY",
    "BsaBI" -> "GATNNNNATC",
    "FseI" -> "GGCCGGCC",
    "BsiHKAI" -> "GWGCWC",
    "Cly7489II" -> "AAAAGRG",
    "SanDI" -> "GGGWCCC",
    "Sse8387I" -> "CCTGCAGG",
    "FaiI" -> "YATR",
    "SaqAI" -> "TTAA",
    "SimI" -> "GGGTC",
    "NspI" -> "RCATGY",
    "LpnI" -> "RGCGCY",
    "AlwFI" -> "GAAAYNNNNNRTG",
    "BfaI" -> "CTAG",
    "PspLI" -> "CGTACG",
    "BalI" -> "TGGCCA",
    "Bsp3004IV" -> "CCGCAT",
    "BpiI" -> "GAAGAC",
    "Eco32I" -> "GATATC",
    "BshNI" -> "GGYRCC",
    "BseDI" -> "CCNNGG",
    "MseI" -> "TTAA",
    "Psp124BI" -> "GAGCTC",
    "PliMI" -> "CGCCGAC",
    "Jma19592II" -> "GRGCRAC",
    "BsgI" -> "GTGCAG",
    "BmcAI" -> "AGTACT",
    "BlnI" -> "CCTAGG",
    "AceIII" -> "CAGCTC",
    "BstMAI" -> "GTCTC",
    "PcsI" -> "WCGNNNNNNNCGW",
    "BmgT120I" -> "GGNCC",
    "AsiGI" -> "ACCGGT",
    "PstI" -> "CTGCAG",
    "MspA1I" -> "CMGCKG",
    "Bsp68I" -> "TCGCGA",
    "TstI" -> "CACNNNNNNTCC",
    "AcyI" -> "GRCGYC",
    "BbrPI" -> "CACGTG",
    "UbaF14I" -> "CCANNNNNTCG",
    "BanII" -> "GRGCYC",
    "BtsCI" -> "GGATG",
    "PalAI" -> "GGCGCGCC",
    "SfaAI" -> "GCGATCGC",
    "TspARh3I" -> "GRACGAC",
    "VspI" -> "ATTAAT",
    "MspCI" -> "CTTAAG",
    "BlsI" -> "GCNGC",
    "ApyPI" -> "ATCGAC",
    "WviI" -> "CACRAG",
    "CauII" -> "CCSGG",
    "HspAI" -> "GCGC",
    "SstE37I" -> "CGAAGAC",
    "CspI" -> "CGGWCCG",
    "PkrI" -> "GCNGC",
    "MjaIV" -> "GTNNAC",
    "PluTI" -> "GGCGCC",
    "DrdI" -> "GACNNNNNNGTC",
    "FblI" -> "GTMKAC",
    "BpuEI" -> "CTTGAG",
    "Psp1406I" -> "AACGTT",
    "BstF5I" -> "GGATG",
    "Eco47III" -> "AGCGCT",
    "Lsp1109I" -> "GCAGC",
    "BsuTUI" -> "ATCGAT",
    "Sbo46I" -> "TGAAC",
    "Bsp119I" -> "TTCGAA",
    "MreI" -> "CGCCGGCG",
    "PciI" -> "ACATGT",
    "GlaI" -> "GCGC",
    "HpyUM037X" -> "TNGGNAG|GTGGNAG",
    "CfoI" -> "GCGC",
    "BanLI" -> "RTCAGG",
    "AbaSI" -> "C",
    "EgeI" -> "GGCGCC",
    "BstYI" -> "RGATCY",
    "FtnUV" -> "GAAACA",
    "Bbr7I" -> "GAAGAC",
    "Sau3AI" -> "GATC",
    "AsuHPI" -> "GGTGA",
    "TatI" -> "WGTACW",
    "TspDTI" -> "ATGAA",
    "BseMI" -> "GCAATG",
    "PvuII" -> "CAGCTG",
    "BsaAI" -> "YACGTR",
    "Eco130I" -> "CCWWGG",
    "SspMI" -> "CTAG",
    "ScrFI" -> "CCNGG",
    "MalI" -> "GATC",
    "CviQI" -> "GTAC",
    "Cdi11397I" -> "GCGCAG",
    "BstAFI" -> "CTTAAG",
    "PacI" -> "TTAATTAA",
    "Hpy8I" -> "GTNNAC",
    "MhlI" -> "GDGCHC",
    "BstEII" -> "GGTNACC",
    "Bsh1285I" -> "CGRYCG",
    "NgoMIV" -> "GCCGGC",
    "BstKTI" -> "GATC",
    "HpyAV" -> "CCTTC",
    "BspMAI" -> "CTGCAG",
    "Cdu23823II" -> "GTGAAG",
    "FauI" -> "CCCGC",
    "AcsI" -> "RAATTY",
    "Eco91I" -> "GGTNACC",
    "MmeI" -> "TCCRAC",
    "BisI" -> "GCNGC",
    "BfaSII" -> "GANGGAG",
    "AfaI" -> "GTAC",
    "NmeAIII" -> "GCCGAG",
    "Sse8647I" -> "AGGWCCT",
    "HgiAI" -> "GWGCWC",
    "VpaK11AI" -> "GGWCC",
    "ChaI" -> "GATC",
    "Cla11845III" -> "GCGAA",
    "LweI" -> "GCATC",
    "AanI" -> "TTATAA",
    "TaqI" -> "TCGA",
    "Cal14237I" -> "GGTTAG",
    "TspRI" -> "CASTG",
    "BbvCI" -> "CCTCAGC",
    "FspAI" -> "RTGCGCAY",
    "BstNSI" -> "RCATGY",
    "AhyYL17I" -> "YAAMGAG",
    "CspAI" -> "ACCGGT",
    "AloI" -> "GAACNNNNNNTCC",
    "BstNI" -> "CCWGG",
    "ApoI" -> "RAATTY",
    "Mox20I" -> "TGGCCA",
    "TaiI" -> "ACGT",
    "PasI" -> "CCCWGGG",
    "SrfI" -> "GCCCGGGC",
    "BtsI" -> "GCAGTG",
    "PflFI" -> "GACNNNGTC",
    "BssT1I" -> "CCWWGG",
    "MroNI" -> "GCCGGC",
    "PspXI" -> "VCTCGAGB",
    "BmiI" -> "GGNNCC",
    "MaeIII" -> "GTNAC",
    "DpnI" -> "GATC",
    "BetI" -> "WCCGGW",
    "BstHHI" -> "GCGC",
    "MspJI" -> "CNNR",
    "KspAI" -> "GTTAAC",
    "BstMCI" -> "CGRYCG",
    "MstI" -> "TGCGCA",
    "PfoI" -> "TCCNGGA",
    "HpaII" -> "CCGG",
    "BstV2I" -> "GAAGAC",
    "BsrDI" -> "GCAATG",
    "DriI" -> "GACNNNNNGTC",
    "CciI" -> "TCATGA",
    "PstNI" -> "CAGNNNCTG",
    "AhdI" -> "GACNNNNNGTC",
    "BstX2I" -> "RGATCY",
    "CdiI" -> "CATCG",
    "Bpu1102I" -> "GCTNAGC",
    "MteI" -> "GCGCNGCGC",
    "BglII" -> "AGATCT",
    "Lba2029III" -> "CYAAANG",
    "AgeI" -> "ACCGGT",
    "Bsc4I" -> "CCNNNNNNNGG",
    "Eco57MI" -> "CTGRAG",
    "BstSCI" -> "CCNGG",
    "XmiI" -> "GTMKAC",
    "DraRI" -> "CAAGNAC",
    "Van91I" -> "CCANNNNNTGG",
    "AflIII" -> "ACRYGT",
    "BccI" -> "CCATC",
    "Tth111II" -> "CAARCA",
    "EheI" -> "GGCGCC",
    "Mcr10I" -> "GAAGNNNNNCTC",
    "EciI" -> "GGCGGA",
    "Hin4II" -> "CCTTC",
    "Zsp2I" -> "ATGCAT",
    "MlsI" -> "TGGCCA",
    "SexAI" -> "ACCWGGT",
    "XhoI" -> "CTCGAG",
    "RsaNI" -> "GTAC",
    "FmuI" -> "GGNCC",
    "EcoO65I" -> "GGTNACC",
    "ZrmI" -> "AGTACT",
    "SspI" -> "AATATT",
    "SstI" -> "GAGCTC",
    "KflI" -> "GGGWCCC",
    "Mly113I" -> "GGCGCC",
    "MvaI" -> "CCWGG",
    "PsuGI" -> "BBCGD",
    "BmeT110I" -> "CYCGRG",
    "SspD5I" -> "GGTGA",
    "BaeI" -> "ACNNNNGTAYC",
    "PaeR7I" -> "CTCGAG",
    "Ple19I" -> "CGATCG",
    "HindII" -> "GTYRAC",
    "TscAI" -> "CASTG",
    "BslI" -> "CCNNNNNNNGG",
    "KasI" -> "GGCGCC",
    "BfrI" -> "CTTAAG",
    "BstXI" -> "CCANNNNNNTGG",
    "GdiII" -> "CGGCCR",
    "BmsI" -> "GCATC",
    "PpuMI" -> "RGGWCCY",
    "RdeGBI" -> "CCGCAG",
    "CviAII" -> "CATG",
    "Eco57I" -> "CTGAAG",
    "LmnI" -> "GCTCC",
    "BsaJI" -> "CCNNGG",
    "CjePI" -> "CCANNNNNNNTC",
    "AatII" -> "GACGTC",
    "CdpI" -> "GCGGAG",
    "NarI" -> "GGCGCC",
    "BtgZI" -> "GCGATG",
    "TssI" -> "GAGNNNCTC",
    "Cau10061II" -> "GTTAAT",
    "CciNI" -> "GCGGCCGC",
    "SpoDI" -> "GCGGRAG",
    "BloAII" -> "GAGGAC",
    "BclI" -> "TGATCA",
    "NheI" -> "GCTAGC",
    "HphI" -> "GGTGA",
    "TauI" -> "GCSGC",
    "PlaDI" -> "CATCAG",
    "HaeII" -> "RGCGCY",
    "EcoT38I" -> "GRGCYC",
    "BssECI" -> "CCNNGG",
    "Aba6411II" -> "CRRTAAG",
    "AchA6III" -> "AGCCAG",
    "Bst2UI" -> "CCWGG",
    "Yps3606I" -> "CGGAAG",
    "BmrI" -> "ACTGGG",
    "PmeS132I" -> "GACGAG",
    "MroXI" -> "GAANNNNTTC",
    "AoxI" -> "GGCC",
    "HinfI" -> "GANTC",
    "SdeOSI" -> "GACNNNNRTGA",
    "BspANI" -> "GGCC",
    "AscI" -> "GGCGCGCC",
    "AarI" -> "CACCTGC",
    "GluI" -> "GCNGC",
    "SmlI" -> "CTYRAG",
    "CjeI" -> "CCANNNNNNGT",
    "SmiI" -> "ATTTAAAT",
    "CpoI" -> "CGGWCCG",
    "PmaCI" -> "CACGTG",
    "BsiWI" -> "CGTACG",
    "BssNI" -> "GRCGYC",
    "XbaI" -> "TCTAGA",
    "StyD4I" -> "CCNGG",
    "AcuI" -> "CTGAAG",
    "EcoNI" -> "CCTNNNNNAGG",
    "Psp6I" -> "CCWGG",
    "Aor13HI" -> "TCCGGA",
    "SgeI" -> "CNNG",
    "Cba16038I" -> "CCTNAYNC",
    "SgrTI" -> "CCDS",
    "TaqIII" -> "CACCCA",
    "SauI" -> "CCTNAGG",
    "AquIV" -> "GRGGAAG",
    "Ppu21I" -> "YACGTR",
    "AspJHL3II" -> "CGCCCAG",
    "BauI" -> "CACGAG",
    "SacII" -> "CCGCGG",
    "PdmI" -> "GAANNNNTTC",
    "BstSFI" -> "CTRYAG",
    "Acc16I" -> "TGCGCA",
    "BsmBI" -> "CGTCTC",
    "Bst4CI" -> "ACNGT",
    "BseX3I" -> "CGGCCG",
    "CjeP659IV" -> "CACNNNNNNNGAA",
    "Eam1104I" -> "CTCTTC",
    "ClaI" -> "ATCGAT",
    "AsiSI" -> "GCGATCGC",
    "PshAI" -> "GACNNNNGTC",
    "RleAI" -> "CCCACA",
    "MvnI" -> "CGCG",
    "PctI" -> "GAATGC",
    "Hpy99XXII" -> "TCANNNNNNTRG",
    "Sfr274I" -> "CTCGAG",
    "ZraI" -> "GACGTC",
    "Bst6I" -> "CTCTTC",
    "BseAI" -> "TCCGGA",
    "AxyI" -> "CCTNAGG",
    "BcgI" -> "CGANNNNNNTGC",
    "BfmI" -> "CTRYAG",
    "Msp20I" -> "TGGCCA",
    "UbaF13I" -> "GAGNNNNNNCTGG",
    "Sen1736II" -> "GATCAG",
    "YkrI" -> "C",
    "MspI" -> "CCGG",
    "PspN4I" -> "GGNNCC",
    "PspFI" -> "CCCAGC",
    "NcoI" -> "CCATGG",
    "HpaI" -> "GTTAAC",
    "NsiI" -> "ATGCAT",
    "DdeI" -> "CTNAG",
    "Eco52I" -> "CGGCCG",
    "BciVI" -> "GTATCC",
    "SenTFIV" -> "GATCAG",
    "BssHII" -> "GCGCGC",
    "PshBI" -> "ATTAAT",
    "MflI" -> "RGATCY",
    "Bsh1236I" -> "CGCG",
    "BaeGI" -> "GKGCMC",
    "Pfl23II" -> "CGTACG",
    "Tru1I" -> "TTAA",
    "RseI" -> "CAYNNNNRTG",
    "PpsI" -> "GAGTC",
    "BdaI" -> "TGANNNNNNTCA",
    "Eco147I" -> "AGGCCT",
    "BstMBI" -> "GATC",
    "PspCI" -> "CACGTG",
    "BspPI" -> "GGATC",
    "LpnPI" -> "CCDG",
    "SfcI" -> "CTRYAG",
    "AspLEI" -> "GCGC",
    "MssI" -> "GTTTAAAC",
    "BspTI" -> "CTTAAG",
    "HpySE526I" -> "ACGT",
    "BspLU11I" -> "ACATGT",
    "Hpy188I" -> "TCNGA",
    "AbaCIII" -> "CTATCAV",
    "BtuMI" -> "TCGCGA",
    "MabI" -> "ACCWGGT",
    "EcoRV" -> "GATATC",
    "Sno506I" -> "GGCCGAG",
    "NdeII" -> "GATC",
    "McrI" -> "CGRYCG",
    "FalI" -> "AAGNNNNNCTT",
    "Hsp92I" -> "GRCGYC",
    "CchII" -> "GGARGA",
    "BstENI" -> "CCTNNNNNAGG",
    "BsaWI" -> "WCCGGW",
    "AccB7I" -> "CCANNNNNTGG",
    "Eco53kI" -> "GAGCTC",
    "FspEI" -> "CC",
    "BceSIV" -> "GCAGC",
    "BssSI" -> "CACGAG",
    "BstAUI" -> "TGTACA",
    "BspTNI" -> "GGTCTC",
    "Lmo911II" -> "TAGRAG",
    "SgrDI" -> "CGTCGACG",
    "AflII" -> "CTTAAG",
    "Bst1107I" -> "GTATAC",
    "BseGI" -> "GGATG",
    "Aco12261II" -> "CCRGAG",
    "BseMII" -> "CTCAG",
    "Psp5II" -> "RGGWCCY",
    "AspBHI" -> "YSCNS",
    "MboII" -> "GAAGA",
    "Bse21I" -> "CCTNAGG",
    "CviRI" -> "TGCA",
    "Pac19842II" -> "CCTTGA",
    "CviKI_1" -> "RGCY",
    "MboI" -> "GATC",
    "TseFI" -> "GTSAC"
  )


}
