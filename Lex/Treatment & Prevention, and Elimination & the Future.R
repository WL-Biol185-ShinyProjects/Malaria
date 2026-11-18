# ============================================================
# SECTIONS REMOVED FROM THE 'What is Malaria?' PAGE
#
# Treatment & Prevention, and Elimination & the Future.
# Each block is a complete tabPanel(...) that uses the mal_*
# helper functions defined near the top of app.R. They can be
# pasted into Final Remarks (or any tabsetPanel) later.
# The tab name 'Treatment & Prevention' linked from the Overview
# card, and the 'Elimination' tab linked to the case study.
# ============================================================

# ======================================================
# TREATMENT & PREVENTION
# ======================================================
tabPanel(
  "Treatment & Prevention",
  br(),
  mal_page(
    
    mal_hero(
      "Curing and preventing malaria",
      "Treatment & Prevention",
      "Malaria is curable when treated promptly, and prevention works best when several approaches are combined."
    ),
    
    mal_heading("Key Points", "Click a card to read more, or explore how well treatments are working."),
    mal_cards(
      mal_card("\U0001F48A", "ACTs: the standard treatment",
               "Artemisinin-based combination therapies for uncomplicated malaria.",
               jump = "tr-uncomplicated"),
      mal_card("\U0001F3E5", "Severe malaria",
               "Intravenous artesunate plus supportive care in hospital.",
               jump = "tr-severe"),
      mal_card("\U0001F6CF\uFE0F", "Nets and spraying",
               "Stopping infected mosquitoes from reaching people.",
               jump = "pr-nets"),
      mal_card("\U0001F489", "Two malaria vaccines",
               "RTS,S/AS01 and R21/Matrix-M for children in endemic areas.",
               jump = "pr-vaccines"),
      mal_card("\U0001F4CA", "Are treatments still working?",
               "Explore WHO therapeutic efficacy studies on the Treatment page.",
               nav = "WHO Therapeutic Efficacy Dashboard", hint = "Open the dashboard \u2192")
    ),
    
    mal_heading("Treatment of Malaria"),
    mal_detail(
      "tr-intro", "Overview", open = TRUE,
      paras(
        "Malaria is a curable disease when appropriate treatment is provided promptly. Treatment aims to eliminate parasites from the body, relieve symptoms, prevent complications, and reduce the risk of further transmission.",
        "The choice of treatment depends on several factors, including the infecting Plasmodium species, severity of infection, geographic location where the infection was acquired, local patterns of drug resistance, pregnancy status, and the patient's underlying health."
      )
    ),
    mal_detail(
      "tr-uncomplicated", "Treatment of uncomplicated malaria",
      paras(
        "Uncomplicated malaria is generally treated with oral antimalarial medications. For many infections caused by P. falciparum, artemisinin-based combination therapies, commonly called ACTs, are recommended.",
        "ACTs combine an artemisinin derivative with a second antimalarial medication that has a longer duration of action. Artemisinin derivatives rapidly reduce the number of malaria parasites in the bloodstream, while the partner medication helps eliminate remaining parasites and reduce the likelihood of treatment failure.",
        "Examples of ACTs include artemether-lumefantrine and artesunate-amodiaquine. The appropriate regimen depends on national guidelines, local resistance patterns, and patient-specific considerations. Using combination therapies rather than artemisinin alone helps reduce the risk of resistance developing."
      )
    ),
    mal_detail(
      "tr-severe", "Treatment of severe malaria",
      paras(
        "Severe malaria requires immediate medical treatment and hospitalization. Intravenous artesunate is the preferred treatment for severe malaria in many clinical settings because it rapidly reduces parasite levels and has been shown to improve survival compared with older treatments such as quinine.",
        "Patients with severe malaria may also require supportive care to manage complications such as severe anemia, hypoglycemia, kidney injury, respiratory distress, and seizures. Depending on the patient's condition, supportive interventions may include blood transfusions, glucose administration, oxygen support, careful fluid management, and kidney replacement therapy.",
        "Once the patient improves and can tolerate oral medication, treatment is completed with an appropriate oral antimalarial regimen."
      )
    ),
    mal_detail(
      "tr-relapsing", "Treatment of relapsing malaria",
      paras(
        "Infections caused by P. vivax and P. ovale require special consideration because these parasites can form dormant liver-stage hypnozoites. Standard blood-stage antimalarial medications eliminate parasites circulating in red blood cells but do not necessarily eliminate dormant parasites in the liver.",
        "To prevent relapse, patients may require an additional medication such as primaquine or tafenoquine. These medications are not suitable for every patient. In particular, they can cause serious hemolysis in individuals with glucose-6-phosphate dehydrogenase deficiency, commonly called G6PD deficiency.",
        "Quantitative G6PD testing is therefore necessary before using these medications, and pregnancy and other clinical factors also affect eligibility."
      )
    ),
    mal_detail(
      "tr-challenges", "Challenges in malaria treatment",
      paras(
        "Although effective medications exist, many patients face barriers to receiving timely treatment. These barriers include limited healthcare infrastructure, long distances to medical facilities, shortages of diagnostic tests, medication costs, inadequate supplies of antimalarial drugs, and delayed recognition of symptoms.",
        "In some regions, counterfeit or substandard medications also threaten treatment effectiveness. The emergence of drug-resistant malaria parasites further complicates treatment and highlights the importance of continued surveillance and development of new therapies."
      )
    ),
    mal_source("Sources: WHO, Guidelines for Malaria; CDC, Treatment of Malaria. This page is general information, not medical advice. Treatment should follow a clinician and current national guidance."),
    
    mal_heading(
      "Prevention and Control",
      "Preventing malaria requires strategies that reduce contact between humans and infected mosquitoes, interrupt parasite transmission, and protect vulnerable populations."
    ),
    mal_detail(
      "pr-nets", "Insecticide-treated mosquito nets",
      paras(
        "Insecticide-treated mosquito nets are among the most widely used malaria prevention tools. These nets provide a physical barrier between sleeping individuals and mosquitoes while also exposing mosquitoes to insecticides that can kill or repel them. Long-lasting insecticidal nets are designed to remain effective through repeated use over an extended period.",
        "Mosquito nets are especially important because many malaria-transmitting Anopheles mosquitoes feed during nighttime hours, when people are sleeping. Widespread distribution and consistent use of insecticide-treated nets have contributed substantially to reductions in malaria transmission and mortality.",
        "However, the effectiveness of mosquito nets depends on appropriate use, physical condition, insecticide effectiveness, and local mosquito behavior."
      )
    ),
    mal_detail(
      "pr-irs", "Indoor residual spraying",
      paras(
        "Indoor residual spraying involves applying insecticides to interior walls and other surfaces where mosquitoes commonly rest. When susceptible mosquitoes land on treated surfaces, they may absorb a lethal dose of insecticide. This approach can reduce mosquito survival and interrupt malaria transmission.",
        "Indoor residual spraying is particularly effective in settings where malaria-transmitting mosquitoes frequently rest indoors after feeding. However, the effectiveness of spraying depends on insecticide susceptibility, the behavior of local mosquito species, and the quality and frequency of spraying programs."
      )
    ),
    mal_detail(
      "pr-environment", "Environmental management",
      paras(
        "Environmental management seeks to reduce mosquito breeding opportunities and limit human exposure to mosquito populations. Many Anopheles mosquitoes lay their eggs in water, although preferred breeding habitats differ between species.",
        "Environmental interventions may include improving drainage, managing standing water, modifying irrigation practices, and maintaining water infrastructure. These strategies must be adapted to local ecological conditions because mosquitoes can reproduce in a variety of natural and human-created habitats.",
        "Environmental management is most effective when integrated with other prevention measures rather than used as a replacement for insecticide-treated nets or medical interventions."
      )
    ),
    mal_detail(
      "pr-medicines", "Preventive antimalarial medications",
      paras(
        "Antimalarial medications can be used to prevent infection or clinical illness among individuals at elevated risk. Travelers visiting malaria-endemic regions may be prescribed chemoprophylaxis before, during, and after travel, depending on the medication. Common options include atovaquone-proguanil, doxycycline, and mefloquine, although suitability varies according to destination, resistance patterns, and individual medical circumstances.",
        "Preventive medication is also used in some endemic populations. Intermittent preventive treatment during pregnancy involves administering antimalarial medication at specified intervals to reduce the risk of malaria-related maternal and fetal complications. Seasonal malaria chemoprevention is used in eligible areas where malaria transmission is highly seasonal, particularly among young children.",
        "These approaches help reduce illness and mortality among vulnerable populations."
      )
    ),
    mal_detail(
      "pr-vaccines", "Malaria vaccines",
      paras(
        "Malaria vaccine development represents an important advancement in infectious disease prevention. Unlike many vaccines targeting viruses or bacteria, malaria vaccines must contend with the complex biology of a eukaryotic parasite that changes developmental stages and expresses different proteins throughout its life cycle.",
        "Two malaria vaccines, RTS,S/AS01 and R21/Matrix-M, have been recommended by the World Health Organization for preventing P. falciparum malaria in children living in endemic areas, particularly those with moderate to high transmission. Both vaccines target the circumsporozoite protein found on the surface of malaria sporozoites. By stimulating immune responses against this protein, the vaccines help reduce the likelihood that parasites will successfully establish infection in the liver.",
        "Although these vaccines can reduce malaria illness and severe outcomes, they do not provide complete protection. Consequently, vaccination is intended to complement existing malaria prevention strategies, including insecticide-treated nets, preventive medications, early diagnosis, and treatment."
      )
    ),
    mal_detail(
      "pr-community", "Community education and public health programs",
      paras(
        "Community engagement is essential for successful malaria prevention. Educational programs can improve awareness of malaria symptoms, encourage early medical evaluation, promote consistent mosquito-net use, and increase understanding of preventive medications and vaccination.",
        "Community health workers often play an important role in malaria control, particularly in rural areas where access to formal healthcare facilities may be limited. Programs that involve local communities in planning and implementation may be better positioned to address cultural practices, logistical barriers, and concerns about preventive interventions.",
        "Successful malaria control therefore depends not only on scientific advancements but also on public trust, education, healthcare accessibility, and sustained public health investment."
      )
    ),
    mal_source("Sources: WHO, Malaria Fact Sheet; WHO, Malaria Vaccines; CDC, Preventing Malaria.")
  )
)

# ======================================================
# ELIMINATION & THE FUTURE
# ======================================================
tabPanel(
  "Elimination & the Future",
  br(),
  mal_page(
    
    mal_hero(
      "Looking ahead",
      "Elimination & the Future",
      "Countries have eliminated malaria, and new vaccines, medicines, and tools are on the way."
    ),
    
    mal_heading("Malaria Elimination and Eradication", "Click a card to read more."),
    mal_cards(
      mal_card("\U0001F3AF", "Elimination vs. eradication",
               "Zero local transmission in one country vs. zero worldwide.",
               jump = "el-difference"),
      mal_card("\U0001F4DC", "WHO certification",
               "Three consecutive years without indigenous transmission.",
               jump = "el-certification"),
      mal_card("\U0001F3C6", "Success stories",
               "Sri Lanka, Cabo Verde, Egypt, Georgia, Suriname, and Timor-Leste.",
               jump = "el-success"),
      mal_card("\U0001F9ED", "Why eradication is hard",
               "Complex biology, resistance, and unequal healthcare access.",
               jump = "el-challenges"),
      mal_card("\U0001F1EA\U0001F1FA", "Elimination in Europe",
               "See how malaria disappeared across the WHO European Region.",
               nav = "WHO European Region Heat Map", hint = "Open the case study \u2192")
    ),
    mal_detail(
      "el-difference", "Elimination versus eradication", open = TRUE,
      paras(
        "Malaria elimination and malaria eradication are related but distinct public health goals. Malaria elimination refers to interrupting indigenous malaria transmission within a defined geographic area, usually a country. Malaria eradication refers to the permanent reduction of worldwide malaria transmission to zero.",
        "Elimination can be achieved in individual countries while malaria continues to circulate elsewhere."
      )
    ),
    mal_detail(
      "el-how", "How countries eliminate malaria",
      paras(
        "Malaria elimination requires sustained reductions in transmission through effective prevention, diagnosis, treatment, and surveillance. As malaria cases become less common, public health programs increasingly focus on identifying individual infections, investigating their sources, and preventing further transmission.",
        "Surveillance becomes especially important because even a small number of infections can sustain or reintroduce malaria transmission under suitable conditions."
      )
    ),
    mal_detail(
      "el-certification", "WHO certification of malaria elimination",
      paras(
        "The World Health Organization certifies countries as malaria-free after they demonstrate that indigenous malaria transmission has been interrupted nationwide for at least three consecutive years. Countries must also demonstrate the capacity to prevent malaria transmission from becoming re-established.",
        "WHO certification recognizes important public health achievements, but malaria-free countries must continue monitoring imported infections and maintaining prevention systems."
      )
    ),
    mal_detail(
      "el-success", "Successful elimination efforts",
      paras(
        "Several countries have successfully eliminated malaria through sustained public health programs. Examples include Sri Lanka, which received WHO malaria-free certification in 2016, and Cabo Verde and Egypt, which received certification in 2024. Georgia, Suriname, and Timor-Leste received WHO malaria-free certification in 2025.",
        "These achievements demonstrate that malaria elimination is possible in different environmental and socioeconomic settings."
      )
    ),
    mal_detail(
      "el-challenges", "Challenges to global eradication",
      paras(
        "Global malaria eradication remains difficult because of the parasite's complex biology, mosquito diversity, asymptomatic infections, drug resistance, and unequal healthcare access. The ability of P. vivax and P. ovale to produce dormant liver-stage infections creates additional challenges.",
        "Population movement can also reintroduce parasites into areas where transmission has been interrupted. Long-term political commitment, international cooperation, and sustained funding are therefore essential for achieving and maintaining elimination."
      )
    ),
    mal_source("Source: WHO, Global Malaria Observatory."),
    
    mal_heading(
      "Current Research and the Future of Malaria Prevention",
      "Existing interventions do not provide complete protection and can be undermined by resistance, so research continues on several fronts."
    ),
    mal_cards(
      mal_card("\U0001F489", "Better vaccines",
               "Targeting more stages of the parasite life cycle.",
               jump = "fu-vaccines"),
      mal_card("\U0001F48A", "New medicines",
               "Novel targets, fewer doses, and dormant liver stages.",
               jump = "fu-medicines"),
      mal_card("\U0001F9EC", "Genetic mosquito control",
               "Promising, but with ecological and ethical questions.",
               jump = "fu-genetic"),
      mal_card("\U0001F52C", "Improved diagnostics",
               "Faster, more sensitive, and more affordable tests.",
               jump = "fu-diagnostics"),
      mal_card("\U0001F91D", "Integrated control",
               "Combining tools with healthcare access and cooperation.",
               jump = "fu-integrated")
    ),
    mal_detail(
      "fu-vaccines", "Development of new malaria vaccines",
      paras(
        "The introduction of RTS,S/AS01 and R21/Matrix-M represents a major advancement in malaria prevention. However, researchers continue working to improve vaccine effectiveness, durability, and accessibility.",
        "Future vaccines may target different stages of the parasite life cycle, including liver-stage parasites, blood-stage parasites, and sexual-stage parasites involved in mosquito transmission. Vaccines targeting multiple parasite stages could potentially provide broader protection."
      )
    ),
    mal_detail(
      "fu-medicines", "New antimalarial medications",
      paras(
        "Researchers are investigating new antimalarial compounds that act through different biological mechanisms. Developing medications with novel targets is especially important because resistance threatens the long-term effectiveness of existing therapies.",
        "Some research focuses on medications capable of eliminating parasites with fewer doses, while other efforts target dormant liver-stage parasites or transmission-associated stages."
      )
    ),
    mal_detail(
      "fu-genetic", "Genetic mosquito-control strategies",
      paras(
        "Genetic approaches to mosquito control are being investigated as potential tools for reducing malaria transmission. Some strategies aim to reduce mosquito populations, while others seek to make mosquitoes less capable of transmitting malaria parasites. Gene-drive technologies are one area of research because they may allow certain inherited traits to spread through mosquito populations.",
        "However, these technologies raise important ecological, ethical, and governance questions. Researchers must consider potential effects on ecosystems, the possibility of unintended consequences, and the importance of community involvement before large-scale implementation."
      )
    ),
    mal_detail(
      "fu-diagnostics", "Improved diagnostic technologies",
      paras(
        "Research is also focused on developing faster, more sensitive, and more affordable malaria diagnostic tools. New molecular methods and portable technologies may improve detection of low-density infections, particularly in areas with limited laboratory infrastructure.",
        "Detecting asymptomatic infections is especially important for elimination programs because infected individuals without symptoms may still contribute to transmission."
      )
    ),
    mal_detail(
      "fu-integrated", "Integrated malaria control",
      paras(
        "The future of malaria prevention will likely depend on combining multiple approaches rather than relying on a single intervention. Vaccines, mosquito control, preventive medications, early diagnosis, effective treatment, and surveillance can work together to reduce transmission.",
        "Scientific advancements must also be accompanied by improvements in healthcare access, public health infrastructure, education, and international cooperation. Ultimately, the goal of malaria research is not simply to develop new medical technologies but to ensure that effective interventions reach the populations most affected by the disease."
      )
    ),
    
    div(
      class = "mal-callout",
      h3("Why Malaria Remains a Global Health Challenge"),
      paras(
        "Malaria is a complex infectious disease shaped by interactions among parasites, mosquito vectors, human immune responses, environmental conditions, and socioeconomic factors. Although effective treatments and preventive interventions exist, malaria continues to cause substantial illness and mortality worldwide.",
        "The biological complexity of Plasmodium parasites contributes to challenges involving diagnosis, treatment, vaccine development, and drug resistance. At the same time, poverty, healthcare inequality, political instability, and environmental changes influence who becomes infected and whether individuals can access appropriate care.",
        "The disproportionate burden of malaria among young children and populations living in sub-Saharan Africa demonstrates the importance of addressing health disparities alongside scientific and medical challenges. Advancements in vaccines, antimalarial medications, diagnostic technologies, and mosquito-control strategies provide important opportunities for reducing malaria transmission. However, these interventions must be supported by sustained public health investment, accessible healthcare systems, community engagement, and international cooperation.",
        "The continued fight against malaria illustrates a central challenge in global health: the existence of effective medical interventions does not guarantee that they will reach everyone who needs them. Reducing malaria-related illness and mortality therefore requires both scientific innovation and a commitment to addressing the social and structural conditions that allow preventable diseases to persist."
      )
    )
  )
)