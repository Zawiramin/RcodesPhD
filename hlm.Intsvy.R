#' HLM with 3 data, STUDENT, SCHOOL, and TEACHER QUESTIONNAIRE

studentFile <- "CY6_MS_CM2_STU_QQQ.sav"
schoolFile <- "CY6_MS_CM2_SCH_QQQ.sav"

#'---------------------------------#
#'---------------------------------#
stuLatVar <- c(
  "JOYSCIE",
  "INTBRSCI",
  #'----------------------------#
  #' 2. Sense of belonging
  "BELONG",
  #'----------------------------#
  #' 3. Science Learning In School
  "DISCLISCI",
  ("IBTEACH"),
  ("TEACHSUP"),
  ("TDTEACH"),
  ("PERFEED"),
  ("ADINST"),
  ("INSTSCIE"),
  #'----------------------------#
  #' 4. Science self-efficacy 
  ("SCIEEFF"),"EPIST","SCIEACT",
  #'----------------------------#
  #' 5. Household possessions
  "CULTPOSS","HEDRES","WEALTH","ICTRES",
  "HOMEPOS",
  #'----------------------------#
  #' 6. Environmental awareness and optimism
  #' Environmental awareness 
  ("ENVAWARE"),
  #' Environmental optimism 
  ("ENVOPT"),
  
  "ANXTEST", 
  "MOTIVAT",
  
  "COOPERATE", 
  "CPSVALUE",
  
  "EMOSUPS"
)

#'---------------------------------#
#'---------------------------------#
schLatVar <- c(
  "SCHSIZE","CLSIZE","SCHLTYPE",
  #School Leadership #Educational leadership (LEAD)
  "LEAD",
  #Curricular development (LEADCOM)
  "LEADCOM",
  #Instructional leadership (LEADINST)
  "LEADINST",
  #Professional development (LEADPD)
  "LEADPD",
  #Teachers participation (LEADTCH)
  "LEADTCH",
  
  #School Resources
  #Shortage of educational material (EDUSHORT)
  "EDUSHORT",
  #Shortage of educational staff (STAFFSHORT)
  "STAFFSHORT",
  
  #School Climate
  #Student-related factors affecting school climate (STUBEHA)
  "STUBEHA",
  #Teacher-related factors affecting school climate (TEACHBEHA)
  "TEACHBEHA"
)

                         
#'---------------------------------#
#' Final merged datasets
#'---------------------------------#
stuSchFile<- pisa.select.merge(student.file = studentFile,
                                  school.file = schoolFile,
                                    countries = "MYS",
                                    student = stuLatVar,
                                    school = schLatVar,
                               )    

stuTecSchFile[,table("CNT")]
table(stuTecSchFile$CNTRYID)
table(pisa15Tch$CNTRYID)+table(pisaSchM$CNTRYID)+table(pisa15Mas$CNTRYID)

