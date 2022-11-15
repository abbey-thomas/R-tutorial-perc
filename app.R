library(shiny)
library(shinyjs)
library(shinyalert)
library(shinyWidgets)
library(speechcollectr)

ui <- fluidPage(
  useShinyjs(),

  # Add an invisible clock that will run continuously
  # and keep the experiment from timing out and disconnecting.
  # Make this element fully transparent
  div(style = "color:rgba(0,0,0,0);",

      # It will need to be reactive (updated throughout the experiment)
      # so we'll make it an output value printed with the server code.
      textOutput("clock")),

  # Some general formatting to make sure everything is centered and at a reasonable width----
  fluidRow(

    ## Note: Width and offset are defined in columns, and there are 12 columns in a row----
    column(width = 8,
           offset = 2,

           # Each portion of the experiment is contained in a separate tab, but the tab titles are invisible to participants----
           tabsetPanel(id = "tabs", selected = "entry", type = "hidden",

                       ## Make the first tab the title page, clearly stating what the experiment is----
                       tabPanelBody(value = "entry",
                                    div(id = "title", style = "text-align:center",
                                        HTML("<h2><em>Welcome to...</em><h2>"),
                                        h1("Emotional Voices:"),
                                        h3("A Study of Emotional Ways of Speaking"),
                                        br(),
                                        br(),
                                        actionButton(inputId = "enter",
                                                     label = "Enter Study"))
                       ),

                       ## Tab 2: Consent----
                       tabPanelBody(value = "consent",
                                    consentUI(title = "Do you consent to participate?")),

                       # Tab 3: PIN----
                       tabPanelBody(value = "show_pin",
                                    div(id = "pinDiv", style = "text-align:center;",
                                        h3("If you had previously begun the experiment and were already assigned an 4-digit ID number, please enter that number here; then click 'Resume'. You will need to complete the practice recording & equipment check again, then you may continue from the recording you made last."),
                                        textInput(inputId = "resume_pin",
                                                  label = "Your ID Number:"),
                                        actionButton("check_pin", "Resume"),
                                        br(),
                                        h3("Otherwise, click 'Begin' to receive a new ID number and begin the experiment."),
                                        actionButton("new_pin", "Begin")),
                                    uiOutput("pin_ui")
                       ),

                       ## Tab 4: Headphone screen----
                       tabPanelBody(value = "headphones",
                                    br(),
                                    div(id = "hpTestDiv", style = "text-align:center;",
                                        h1("Equipment Check"),
                                        headphoneTestUI(type = "huggins")
                                    )
                       ),

                       ## Tab 5: Demographic survey----
                       # Remember to use a separate script to prepare 'www/survey.csv' and put it in the www folder
                       tabPanelBody(value = "background",
                                    surveyUI(id = "survey",
                                             questionFile = "www/survey.csv",
                                             title = "Background Information",
                                             notListedLab = "Not listed:")
                       ),

                       ## Tab 6: Experiment instructions----
                       tabPanelBody(value = "instructions",
                                    div(id = "instructDiv", style = "text-align:center",
                                        br(),
                                        h2("Let's Play a Game!"),
                                        br(),
                                        h4("This game will require you to listen to people saying short
                                            sentences with different emotions. With each recording you
                                            listen to, you'll need to note two things: (1) What the talker
                                            said and (2) What emotion they said it in."),
                                        br(),
                                        h4("Click BEGIN when you are ready to begin listening."),
                                        actionButton("begin", "BEGIN"))),

                       ## Tab 6: Transcription----
                       tabPanelBody(value = "transcribe",
                                    div(id = "tranDiv", style = "text-align:center;",
                                        br(),
                                        transcribeUI())
                       ),

                       ## Tab 7: Experimental task  interface----
                       tabPanelBody(value = "task",
                                    div(id = "taskDiv", style = "text-align: center;",
                                        h5("Listen to the recording again and answer the question below."),
                                        br(),
                                        uiOutput("play_stim"),
                                        rateUI(id = "identify",
                                               n_scales = 4,
                                               type = "button"),
                                        br(),
                                        rateUI(id = "rate",
                                               n_scales = 1,
                                               type = "slider")
                                    ),
                                    hidden(actionButton("next_n", "Next Recording"))
                       )
           ),
           # Put progress bars at the bottom of the screen.
           # Outside the tabset so that they appear on both the transcription
           # and rating tabs.
           # But initialize as hidden so they don't
           # confuse the participant at the beginning of the experiment
           hidden(div(id = "progDiv", style = "text-align:center",
                      # Some blank lines to put the progress bars near the bottom of the screen
                      br(),
                      br(),
                      br(),
                      hr(),
                      progressBar(id = "score",
                                  value = 0,
                                  total = 2,
                                  display_pct = TRUE,
                                  status = "success",
                                  title = "Your current score:"),
                      br(),
                      progressBar(id = "trial",
                                  value = 0,
                                  total = 16,
                                  title = "Trials completed:"),
                      HTML("<h6><em>Note: You can earn up to 2 points per trial. The maximum points
                                possible across all 16 trials is 32 points.</em></h6>")))

    )
  )
)


# Server program----
server <- function(input, output, session) {
  # Create a reactive values object that holds the trial number
  # Eventually, the participant's PIN and their set of randomized stimuli
  # will also be stored in this object.
  # All these values can be created/accessed by prefixing the value name with "counter$"
  counter <- reactiveValues(n = 1, attempt = 1)

  # Create a separate reactive values object to hold the outputs from each trial
  out <- reactiveValues(points = 0)

  # When a participant clicks "enter"...
  observeEvent(input$enter, {

    ## Send them to the consent form----
    updateTabsetPanel(session, "tabs", selected = "consent")
    consent <- consentServer(delayResponse = 2000,
                             cons2rec = FALSE,
                             result = "hide",
                             disagreeLab = "Disagree")

    ## When consent is obtained, move to the practice recording----
    ## Note: the consent server function will return the reactive value "consent"
    ## Reactive values are like events or functions, and must be followed by "()"
    ## unless contained in an object created with "shiny::reactiveValues()"
    observeEvent(consent(), {
      updateTabsetPanel(session, "tabs", selected = "show_pin")
    })
  })

  # If the participant is new...
  observeEvent(input$new_pin, {
    hide("pinDiv")

    # Now we create a new pin and store it in the reactive "counter" object
    # so that it will be available outside of this "observeEvent()" command
    counter$pin <- pinGen(reactive = FALSE)

    # Prepare stimuli
    # Just preserve the basename as everything else (extension and filepath)
    # will be specified as needed for each command.
    files <- gsub(".wav", "", list.files(path = "www/perc_stim",
                                         pattern = ".wav"))

    # If randomized here using the base R sample function,
    # the stimuli will be presented in a different random order to each participant
    # sampled without replacement...
    counter$stimuli <- c(sample(files, size = 16))

    # save a record of the order the stimuli were presented to the current participant in
    saveRDS(counter$stimuli, paste0("www/outputs/stimuli", counter$pin,
                                    ".rds"))

    # And create the vector of correct transcriptions
    counter$correct <- ifelse(grepl("1$", counter$stimuli),
                              "Kids are talking by the door.",
                              "Dogs are sitting by the door.")

    # Show the PIN to the participant
    output$pin_ui <- renderUI({
      div(id = "pinDiv2", style = "text-align:center;",
          br(),
          h3("Welcome! Your ID number is: "),
          br(),
          h1(counter$pin),
          br(),
          h3("Please make a note of this number. You may use it to resume the experiment if necessary."),
          h3(em("NOTE: We strongly recommend completing the experiment in one sitting.")),
          actionButton("pin_done", "Click here to Continue.")
      )
    })
  })

  # If the participant is returning...
  observeEvent(input$check_pin, {

    # check to see whether the PIN they entered matches a demographic survey we have already saved
    if (file.exists(paste0("www/outputs/demographics", input$resume_pin, ".rds"))) {

      hide("pinDiv")

      # If the PIN they entered does match data we have stored, put it in the reactive "counter" object
      # so that it will be accessible throughout the experiment
      counter$pin <- as.numeric(input$resume_pin)

      if (file.exists(paste0("www/outputs/completed", counter$pin, ".rds"))) {
        # Change the trial number to the number of the last completed trial
        # if and only if they made it past the first trial
        completed <- readRDS(paste0("www/outputs/completed", counter$pin, ".rds"))
        counter$n <- completed[1] + 1
        out$points <- completed[2]

        updateProgressBar(session,
                          id = "score",
                          value = out$points,
                          total = counter$n*2)

        updateProgressBar(session,
                          id = "trial",
                          value = counter$n,
                          total = 16)
      }

      # Read in the randomized set of stimuli from the participant's session
      # And store the stimuli in the reactive object
      counter$stimuli <- readRDS(paste0("www/outputs/stimuli", counter$pin, ".rds"))

      # And create the vector of correct transcriptions
      counter$correct <- ifelse(grepl("1$", counter$stimuli),
                                "Kids are talking by the door.",
                                "Dogs are sitting by the door.")

      # They will have to check their equipment again, as they might have changed devices.
      # After the practice recording, the participant will resume the experiment from the last completed trial.
      output$pin_ui <- renderUI({
        div(id = "pinDiv2", style = "text-align:center;",
            br(),
            h1("Welcome back!"),
            br(),
            h3("You'll need to complete the headphone check again before resuming the experiment. After you complete the check, you'll go straight to the last trial you completed and proceed from there."),
            actionButton("pin_done", "Click here to Continue.")
        )
      })

    } else {
      # If no data exists for the PIN the participant has entered, send them back to get a new PIN.
      shinyalert(title = "Invalid ID Number",
                 text = "We do not have any data associated with the ID number you
               entered. Please begin the experiment again by requesting a new ID number.",
                 type = "warning",
                 confirmButtonText = "OKAY")
    }
  })

  # Once a new participant has a PIN, move them to the practice recording.
  observeEvent(input$pin_done, {
    updateTabsetPanel(session, "tabs", selected = "headphones")
    hp_test <- headphoneTestServer(type = "huggins",
                                   threshold = 6)
    observe({
      if (req(hp_test())==1) {
        if (!file.exists(paste0("www/outputs/demographics", counter$pin, ".rds"))) {
          updateTabsetPanel(session, "tabs", selected = "background")

          surveyServer(id = "survey",
                       questionFile = "www/survey.csv",
                       result = "hide",
                       outFile = paste0("www/outputs/demographics", counter$pin, ".rds"),
                       notListedLab = "Not listed:")
        } else {
          updateTabsetPanel(session, "tabs", selected = "transcribe")
          showElement("progDiv")

          out$transc <- transcribeServer(audioFile = paste0("www/perc_stim/", counter$stimuli[counter$n],
                                                            ".wav"),
                                         outFile = paste0("www/outputs/",
                                                          counter$pin, "_",
                                                          counter$stimuli[counter$n], ".rds"),
                                         n_play = 4,
                                         result = "hide")
        }
      }
    })
  })

  # Once the participant completes the demographic survey,
  # send them to the instructions for the experiment.
  # Note that for all speechcollectr module functions, the submit button ID will be "submit"
  # prefixed by the value of the "id" argument of the module + a hyphen.
  # To access these buttons, we'll need to treat the submit button id as a character string name/index
  # of an item in the list of inputs.
  # (This is standard practice for accessing inputs created inside other shiny modules,
  # besides those included in speechcollectr)
  observeEvent(input[["survey-submit"]], {
    updateTabsetPanel(session, "tabs", selected = "instructions")
    showElement("progDiv")
  })

  observeEvent(input$begin, {
    updateTabsetPanel(session, "tabs", "transcribe")
    print(counter$correct[counter$n])

    shinyalert(title = "Listen carefully!",
               text = "Play the recording and type exactly what the
                        talker says in the box below. You won't be able to move on
                        until the text you type matches the sentence you hear.
                        You'll only be able to play the recording twice, so listen
                        carefully! You'll be scored based on how many attempts you
                        submit before getting the correct answer. Spelling is
                        important, but case and punctuation can be ignored!",
               type = "info")

    out$transc <- transcribeServer(audioFile = paste0("www/perc_stim/",
                                                      counter$stimuli[counter$n],
                                                      ".wav"),
                                   outFile = paste0("www/outputs/",
                                                    counter$pin, "_",
                                                    counter$stimuli[counter$n], ".rds"),
                                   n_play = 4,
                                   result = "hide")
  })

  observeEvent(input[["transcribe-submit"]], {
    delay(500,
          out$correct <- evalTranscServer(filename = paste0("www/outputs/",
                                                            counter$pin, "_",
                                                            counter$stimuli[counter$n], ".rds"),
                                          correct = counter$correct[counter$n],
                                          attempts = 2,
                                          counter = counter$attempt,
                                          passInputId = "pass",
                                          warnInputId = "warn",
                                          failInputId = "fail"))
  })

  observeEvent(input$pass, {
    updateTabsetPanel(session, "tabs", selected = "task")
    showElement("taskDiv")

    new_points <- ifelse(counter$attempt < 3, 3-counter$attempt, 0)
    out$points <- out$points + new_points

    delay(500, updateProgressBar(session,
                                 id = "score",
                                 value = out$points,
                                 total = counter$n*2))

    # Add the insertion of the audio playback interface
    output$play_stim <- renderUI({
      playBttn(inputId = "play_stim",
               src = paste0("www/perc_stim/", counter$stimuli[counter$n], ".wav"),
               audioId = "stim_audio",
               label = "Listen again.",
               icon = "play")
    })
  })

  observeEvent(input$warn, {
    counter$attempt <- counter$attempt + 1
    out$transc <- transcribeServer(audioFile = paste0("www/perc_stim/",
                                                      counter$stimuli[counter$n],
                                                      ".wav"),
                                   outFile = paste0("www/outputs/",
                                                    counter$pin, "_",
                                                    counter$stimuli[counter$n], ".rds"),
                                   n_play = 4,
                                   result = "hide")
  })

  observeEvent(input$play_stim, {
    emo_list <- sample(list("happy", "sad", "angry", "neutral"), size = 4)
    out$emo_sel <- rateServer(id = "identify",
                              trigger = NULL,
                              n_scales = 4,
                              type = "button",
                              answers = emo_list,
                              pretext = "Click the word that best describes the
                              emotion in the talker's tone of voice in this
                              recording:",
                              direction = "vertical")
  })

  observeEvent(input[["identify-submit"]], {
    hide("identify")
    delay(500, print(out$emo_sel()))
    out$emo_conf <- rateServer(id = "rate",
                               trigger = NULL,
                               n_scales = 1,
                               type = "slider",
                               answers = list(c("Not at all confident",
                                                "Extremely confident")),
                               pretext = paste0("How confident are you that the
                                                  talker used a(n) ",
                                                out$emo_sel(), " tone of voice in this
                                                  recording?"),
    )

  })

  observeEvent(input[["rate-submit"]], {
    hide("taskDiv")
    updateProgressBar(session,
                      id = "trial",
                      value = counter$n,
                      total = 16)

    delay(500, out_df <- data.frame(pin = counter$pin,
                                    trial = counter$n,
                                    audio = counter$stimuli[counter$n],
                                    total_points = out$points,
                                    emotion = paste0(out$emo_sel()),
                                    confidence = paste0(out$emo_conf()),
                                    time = Sys.time()))


    delay(500, saveRDS(out_df, file = paste0("www/outputs/trial", counter$pin, "_", counter$n, ".rds")))
    delay(500, saveRDS(c(counter$n, out$points), paste0("www/outputs/completed", counter$pin, ".rds")))
    showElement("next_n")
  })

  observeEvent(input$next_n, {
    counter$n <- counter$n+1
    out$emo_sel <- NULL
    out$emo_conf <- NULL
    hide("next_n")

    if (counter$n <= length(counter$stimuli)) {
      updateTabsetPanel(session, "tabs", selected = "transcribe")
      out$transc <- transcribeServer(audioFile = paste0("www/perc_stim/", counter$stimuli[counter$n],
                                                        ".wav"),
                                     outFile = paste0("www/outputs/",
                                                      counter$pin, "_",
                                                      counter$stimuli[counter$n], ".rds"),
                                     n_play = 4,
                                     result = "hide")
    } else {
      out$rank <- rankPlayer(score = out$points,
                             playerId = counter$pin,
                             rankFile = "www/perc_rank.rds")

      shinyalert(title = "Experiment Complete!",
                 text = paste0("Great job! Based on your score, you ranked #",
                               out$rank$rank, " out of ", out$rank$out_of, " players. Thank you for
               your participation! Your data has been saved. Please close this
               browser window now."),
                 showConfirmButton = FALSE)
    }
  })
}
shinyApp(ui = ui, server = server)
