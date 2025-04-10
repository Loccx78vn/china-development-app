box::use(
  shiny[...],
  bslib[...],
  htmltools[...],
  jsonlite[...],
  fontawesome[...]
)

#' @export
timelineInput <- function(inputId, label = NULL, choices, selected = NULL, title = NULL, rangeStart = NULL, rangeEnd = NULL) {
  # Default to first value if selected is NULL (to match selectInput behavior)
  if (is.null(selected) && length(choices) > 0) {
    req(FALSE, message = "No item selected, though choices are available.")
  }
  
  # If range not specified, default to min/max of choices
  if (is.null(rangeStart)) rangeStart <- min(as.numeric(choices))
  if (is.null(rangeEnd)) rangeEnd <- max(as.numeric(choices))
  
  # Generate the full range of years (for the continuous timeline)
  Range <- rangeStart:rangeEnd
  fullRange <- sort(unique(c(choices, Range[Range %% 2 == 0])))
  
  # Convert selected to character to ensure consistent comparison
  selected <- as.character(selected)
  choices <- as.character(choices)
  
  # Create the CSS styles
  css <- tags$style(HTML("
    /* Timeline container */
    .timeline-container {
      position: relative;
      max-width: 100%;
      margin: 10px auto 5px;
      padding: 10px 0;
    }
    
    /* The actual timeline (the horizontal ruler) */
    .timeline-container::after {
      content: '';
      position: absolute;
      width: 96%;
      height: 3px;
      background: linear-gradient(90deg, #e9ecef, #d1d7dc, #e9ecef);
      top: 50%;
      left: 2%;
      border-radius: 2px;
      z-index: 1;
      box-shadow: 0 1px 2px rgba(0,0,0,0.1);
    }
    
    /* The markers container */
    .timeline-years {
      display: flex;
      justify-content: space-between;
      position: relative;
      z-index: 2;
      width: 100%;
    }
    
    /* Hide the actual radio buttons */
    .timeline-years input[type='radio'] {
      display: none;
    }
    
    /* Style the labels */
    .timeline-years label {
      display: flex;
      flex-direction: column;
      align-items: center;
      position: relative;
      min-width: 30px;
      transition: transform 0.3s ease;
    }
    
    /* For selectable years, add cursor pointer */
    .timeline-years label.selectable {
      cursor: pointer;
    }
    
    /* Style the year markers (vertical lines) */
    .year-marker {
      width: 1px;
      height: 12px;
      background-color: #adb5bd;
      transition: all 0.3s ease;
      margin-bottom: 5px;
      border-radius: 1px;
      position: relative;
    }
    
    /* Add dot at the intersection with the horizontal line */
    .year-marker::before {
      content: '';
      position: absolute;
      width: 6px;
      height: 6px;
      background-color: #adb5bd;
      border-radius: 50%;
      bottom: -3px;
      left: 50%;
      transform: translateX(-50%);
      transition: all 0.3s ease;
    }
    
    /* For selectable years, make the marker more prominent */
    .selectable .year-marker {
      width: 2px;
      height: 25px;
      background-color: #6c757d;
    }
    
    .selectable .year-marker::before {
      width: 10px;
      height: 10px;
      background-color: #6c757d;
      bottom: -5px;
    }
    
    /* Style the year text (in circles) */
    .year-text {
      font-weight: 500;
      color: #495057;
      transition: all 0.3s ease;
      background-color: #f8f9fa;
      border: 1px solid #6c757d;
      border-radius: 50%;
      width: 45px;
      height: 45px;
      display: flex;
      align-items: center;
      justify-content: center;
      box-shadow: 0 1px 3px rgba(0,0,0,0.1);
      font-size: 0.9rem;
    }
    
    /* For non-selectable years, show just a label */
    .year-label {
      font-size: 0.8rem;
      color: #6c757d;
      margin-top: 5px;
    }
    
    /* Hover effect for selectable year markers */
    .timeline-years label.selectable:hover .year-marker {
      background-color: #0275d8;
      height: 30px;
    }
    
    .timeline-years label.selectable:hover .year-marker::before {
      background-color: #0275d8;
      width: 12px;
      height: 12px;
      box-shadow: 0 0 5px rgba(2, 117, 216, 0.5);
    }
    
    /* Hover effect for selectable year text */
    .timeline-years label.selectable:hover .year-text {
      border-color: #0275d8;
      color: #0275d8;
      background-color: #f0f7ff;
      box-shadow: 0 2px 5px rgba(2, 117, 216, 0.2);
    }
    
    /* Style for selected year */
    .timeline-years input[type='radio']:checked + label {
      transform: translateY(-3px);
    }
    
    .timeline-years input[type='radio']:checked + label .year-marker {
      background-color: #0275d8;
      height: 35px;
      width: 3px;
    }
    
    .timeline-years input[type='radio']:checked + label .year-marker::before {
      background-color: #0275d8;
      width: 12px;
      height: 12px;
      box-shadow: 0 0 6px rgba(2, 117, 216, 0.6);
    }
    
    /* Style for selected year text */
    .timeline-years input[type='radio']:checked + label .year-text {
      color: white;
      font-weight: 600;
      background-color: #0275d8;
      border-color: #0275d8;
      transform: scale(1.1);
      box-shadow: 0 3px 8px rgba(2, 117, 216, 0.3);
    }
    
    /* Make the timeline more responsive */
    @media (max-width: 992px) {
      .timeline-years {
        padding: 0 10px;
      }
      
      .year-text {
        width: 40px;
        height: 40px;
        font-size: 0.85rem;
      }
    }
    
    @media (max-width: 768px) {
      .timeline-years {
        flex-wrap: wrap;
        gap: 15px;
        justify-content: center;
      }
      
      .timeline-container::after {
        display: none;
      }
      
      .selectable .year-marker {
        height: 20px;
      }
      
      .selectable .year-marker::before {
        bottom: auto;
        top: -5px;
      }
      
      /* Hide non-selectable years on mobile */
      .timeline-years label:not(.selectable) {
        display: none;
      }
    }
    
    /* Scale effect on hover */
    .timeline-years label.selectable:hover {
      transform: translateY(-2px);
    }
    
    /* Title styling */
    .timeline-title {
      font-weight: 600;
      margin-bottom: 5px;
      color: #212529;
    }
    
    /* Custom label styling - positioned at top right */
    .timeline-label-container {
      display: flex;
      justify-content: flex-end;
      align-items: center;
      margin-bottom: 5px;
    }
    
    .timeline-label {
      font-weight: bold;
      display: flex;
      align-items: center;
      color: #495057;
    }
    
    .timeline-label i {
      margin-right: 5px;
      color: #0275d8;
    }
  "))
  
  # Create the timeline container
  timeline_container <- div(
    class = "timeline-component",
    css,
    if (!is.null(label) && label != "") {
      div(
        class = "timeline-label-container",
        span(
          class = "timeline-label",
          icon("calendar-alt"),  # Add calendar icon
          label
        )
      )
    },
    if (!is.null(title) && title != "") h5(class = "timeline-title", title),
    div(
      class = "timeline-container",
      div(id = paste0(inputId, "-timeline"), class = "timeline-years")
    )
  )
  
  # JavaScript to create the timeline elements dynamically with improved initialization
  js <- tags$script(HTML(sprintf("
    // Wait for Shiny to be fully connected before initializing
    $(document).on('shiny:connected', function() {
      // Full range of years (for continuous timeline)
      const fullRange = %s;
      
      // Years that are selectable
      const selectableYears = %s;
      
      const timelineContainer = document.getElementById('%s-timeline');
      const inputId = '%s';
      const selectedValue = '%s';
      
      // Create elements for each year in the full range
      fullRange.forEach((year, index) => {
        const yearId = `${inputId}-${year}`;
        const isSelectable = selectableYears.includes(year.toString());
        
        // Create elements for selectable years
        if (isSelectable) {
          // Create input element (radio button)
          const input = document.createElement('input');
          input.type = 'radio';
          input.id = yearId;
          input.name = inputId;
          input.value = year;
          if (year.toString() === selectedValue) input.checked = true;
          
          // Create label
          const label = document.createElement('label');
          label.htmlFor = yearId;
          label.className = 'selectable';
          
          // Create marker div (vertical line)
          const marker = document.createElement('div');
          marker.className = 'year-marker';
          
          // Create year text (in circle)
          const yearText = document.createElement('div');
          yearText.className = 'year-text';
          yearText.textContent = year;
          
          // Append elements
          label.appendChild(marker);
          label.appendChild(yearText);
          
          timelineContainer.appendChild(input);
          timelineContainer.appendChild(label);
          
          // Add Shiny binding for the radio button
          input.addEventListener('change', function() {
            // Send value to Shiny
            Shiny.setInputValue(inputId, this.value);
          });
        } 
        // Create simpler markers for non-selectable years
        else {
          // Create a simple marker with just a line
          const label = document.createElement('label');
          
          // Create marker div (vertical line)
          const marker = document.createElement('div');
          marker.className = 'year-marker';
          
          // Create a simple text label
          const yearLabel = document.createElement('div');
          yearLabel.className = 'year-label';
          yearLabel.textContent = year;
    
              // Append elements
          label.appendChild(marker);
          label.appendChild(yearLabel);
          
          timelineContainer.appendChild(label);
        }
      });
      
      // Initialize the Shiny input with the default selected value
      // Make sure this happens after Shiny is ready
      if (window.Shiny) {
        console.log('Setting initial value: ' + selectedValue);
        Shiny.setInputValue(inputId, selectedValue);
        
        // Force a delayed update to ensure the value is set
        setTimeout(function() {
          Shiny.setInputValue(inputId, selectedValue);
        }, 500);
      }
      
            // Add animation when the component is loaded
        const timeline = document.querySelector('.timeline-container');
        timeline.style.opacity = '0';
        timeline.style.transform = 'translateY(10px)';
        timeline.style.transition = 'opacity 0.5s ease, transform 0.5s ease';
        
        setTimeout(() => {
          timeline.style.opacity = '1';
          timeline.style.transform = 'translateY(0)';
        }, 200);
      });
    ", 
    jsonlite::toJSON(fullRange), 
    jsonlite::toJSON(choices),
    inputId,
    inputId,
    selected
  )))
  
  # Combine the timeline container and JavaScript
  tagList(
    timeline_container,
    js
  )
}



#' @export
shipping_selector_ui <- function(
    inputId,
    label = "Shipping Company",
    choices = NULL,
    selected = NULL,
    multiple = FALSE,
    width = "350px"
) {
  # Default choices if none provided
  if(is.null(choices)) {
    choices <- c("No value return!!!")
  }
  
  # Enhanced styled selectInput with custom CSS
  tagList(
    tags$head(
      tags$style(
        HTML(paste0("
          /* Compact select styling with better alignment */
          #", inputId, "-container", " {
            padding: 8px 10px;
            border-radius: 6px;
            background: #e6f2ff; /* Light blue background */
            box-shadow: 0 2px 5px rgba(0,0,0,0.05);
            margin: 10px auto;
            position: relative;
            border: 1px solid #d1e6ff;
            max-width: ", width, ";
            display: flex;
            align-items: center;
            justify-content: flex-start;
          }
          
          #", inputId, "-header-group", " {
            display: flex;
            align-items: center;
            margin-right: 12px;
            flex-shrink: 0;
          }
          
          #", inputId, "-icon", " {
            margin-right: 6px;
            font-size: 14px;
            color: #4682B4;
            display: flex;
            align-items: center;
            justify-content: center;
          }
          
          #", inputId, "-header", " {
            color: #343a40;
            font-size: 13px;
            font-weight: 600;
            white-space: nowrap;
            flex-shrink: 0;
            line-height: 1;
            padding-top: 1px; /* Fine-tune vertical alignment */
          }
          
          #", inputId, "-select-container", " {
            flex-grow: 1;
            position: relative;
            min-width: 180px; /* Minimum width for the dropdown */
          }
          
          /* Remove default margin from selectize control */
          #", inputId, "-select-container", " .form-group {
            margin-bottom: 0 !important;
          }
          
          #", inputId, "-select-container", " .selectize-input {
            border-radius: 4px !important;
            border: 1px solid #ced4da !important;
            padding: 6px 10px !important; /* Increased padding */
            min-height: 32px !important; /* Increased height */
            box-shadow: none !important;
            font-size: 13px !important;
            display: flex !important;
            align-items: center !important;
          }
          
          #", inputId, "-select-container", " .selectize-input.focus {
            border-color: #80bdff !important;
            box-shadow: 0 0 0 2px rgba(0, 123, 255, 0.15) !important;
          }
          
          #", inputId, "-select-container", " .selectize-dropdown {
            border-radius: 4px !important;
            font-size: 13px !important;
            width: 100% !important; /* Ensure dropdown is full width */
          }
          
          #", inputId, "-select-container", " .selectize-dropdown-content .option {
            padding: 6px 10px !important; /* Increased padding */
          }
        "))
      )
    ),
    div(
      id = paste0(inputId, "-container"),
      class = "compact-select-container",
      div(id = paste0(inputId, "-header-group"), class = "compact-select-header-group",
          div(id = paste0(inputId, "-icon"), class = "compact-select-icon", icon("ship")),
          div(id = paste0(inputId, "-header"), class = "compact-select-header", label)
      ),
      div(id = paste0(inputId, "-select-container"), class = "compact-select",
          selectInput(
            inputId = inputId,
            label = NULL,
            choices = choices,
            selected = selected,
            multiple = multiple,
            width = "100%",
            selectize = TRUE
          )
      )
    )
  )
}

#' @export
timelineInput_test <- function(inputId, choices, selected = NULL, label = NULL, 
                          rangeStart = NULL, rangeEnd = NULL) {
  
  # Ensure choices are character vectors
  choices <- as.character(choices)
  
  # Set default values for range if not provided
  if(is.null(rangeStart)) rangeStart <- min(as.numeric(choices))
  if(is.null(rangeEnd)) rangeEnd <- max(as.numeric(choices))
  
  # Set default selected value if not provided
  if(is.null(selected)) selected <- choices[1]
  
  # Create the full range of years (including non-selectable ones)
  fullRange <- seq(rangeStart, rangeEnd, by = 2)  # Adjust the step as needed
  
  # Convert to JSON for JavaScript
  selectableYearsJSON <- jsonlite::toJSON(choices)
  fullRangeJSON <- jsonlite::toJSON(fullRange)
  selectedJSON <- jsonlite::toJSON(selected)
  
  # Create the HTML for the custom input
  html <- HTML(paste0(
    '<div class="timeline-component">
      <style>
        /* Timeline container */
        .timeline-container {
          position: relative;
          max-width: 100%;
          margin: 10px auto 5px;
          padding: 10px 0;
        }
        
        /* The actual timeline (the horizontal ruler) */
        .timeline-container::after {
          content: \'\';
          position: absolute;
          width: 96%;
          height: 3px;
          background: linear-gradient(90deg, #e9ecef, #d1d7dc, #e9ecef);
          top: 50%;
          left: 2%;
          border-radius: 2px;
          z-index: 1;
          box-shadow: 0 1px 2px rgba(0,0,0,0.1);
        }
        
        /* The markers container */
        .timeline-years {
          display: flex;
          justify-content: space-between;
          position: relative;
          z-index: 2;
          width: 100%;
        }
        
        /* Hide the actual radio buttons */
        .timeline-years input[type=\'radio\'] {
          display: none;
        }
        
        /* Style the labels */
        .timeline-years label {
          display: flex;
          flex-direction: column;
          align-items: center;
          position: relative;
          min-width: 30px;
          transition: transform 0.3s ease;
        }
        
        /* For selectable years, add cursor pointer */
        .timeline-years label.selectable {
          cursor: pointer;
        }
        
        /* Style the year markers (vertical lines) */
        .year-marker {
          width: 1px;
          height: 12px;
          background-color: #adb5bd;
          transition: all 0.3s ease;
          margin-bottom: 5px;
          border-radius: 1px;
          position: relative;
        }
        
        /* Add dot at the intersection with the horizontal line */
        .year-marker::before {
          content: \'\';
          position: absolute;
          width: 6px;
          height: 6px;
          background-color: #adb5bd;
          border-radius: 50%;
          bottom: -3px;
          left: 50%;
          transform: translateX(-50%);
          transition: all 0.3s ease;
        }
        
        /* For selectable years, make the marker more prominent */
        .selectable .year-marker {
          width: 2px;
          height: 25px;
          background-color: #6c757d;
        }
        
        .selectable .year-marker::before {
          width: 10px;
          height: 10px;
          background-color: #6c757d;
          bottom: -5px;
        }
        
        /* Style the year text (in circles) */
        .year-text {
          font-weight: 500;
          color: #495057;
          transition: all 0.3s ease;
          background-color: #f8f9fa;
          border: 1px solid #6c757d;
          border-radius: 50%;
          width: 45px;
          height: 45px;
          display: flex;
          align-items: center;
          justify-content: center;
          box-shadow: 0 1px 3px rgba(0,0,0,0.1);
          font-size: 0.9rem;
        }
        
        /* For non-selectable years, show just a label */
        .year-label {
          font-size: 0.8rem;
          color: #6c757d;
          margin-top: 5px;
        }
        
        /* Hover effect for selectable year markers */
        .timeline-years label.selectable:hover .year-marker {
          background-color: #0275d8;
          height: 30px;
        }
        
        .timeline-years label.selectable:hover .year-marker::before {
          background-color: #0275d8;
          width: 12px;
          height: 12px;
          box-shadow: 0 0 5px rgba(2, 117, 216, 0.5);
        }
        
        /* Hover effect for selectable year text */
        .timeline-years label.selectable:hover .year-text {
          border-color: #0275d8;
          color: #0275d8;
          background-color: #f0f7ff;
          box-shadow: 0 2px 5px rgba(2, 117, 216, 0.2);
        }
        
        /* Style for selected year */
        .timeline-years input[type=\'radio\']:checked + label {
          transform: translateY(-3px);
        }
        
        .timeline-years input[type=\'radio\']:checked + label .year-marker {
          background-color: #0275d8;
          height: 35px;
          width: 3px;
        }
        
        .timeline-years input[type=\'radio\']:checked + label .year-marker::before {
          background-color: #0275d8;
          width: 12px;
          height: 12px;
          box-shadow: 0 0 6px rgba(2, 117, 216, 0.6);
        }
        
        /* Style for selected year text */
        .timeline-years input[type=\'radio\']:checked + label .year-text {
          color: white;
          font-weight: 600;
          background-color: #0275d8;
          border-color: #0275d8;
          transform: scale(1.1);
          box-shadow: 0 3px 8px rgba(2, 117, 216, 0.3);
        }
        
        /* Make the timeline more responsive */
        @media (max-width: 992px) {
          .timeline-years {
            padding: 0 10px;
          }
          
          .year-text {
            width: 40px;
            height: 40px;
            font-size: 0.85rem;
          }
        }
        
        @media (max-width: 768px) {
          .timeline-years {
            flex-wrap: wrap;
            gap: 15px;
            justify-content: center;
          }
          
          .timeline-container::after {
            display: none;
          }
          
          .selectable .year-marker {
            height: 20px;
          }
          
          .selectable .year-marker::before {
            bottom: auto;
            top: -5px;
          }
          
          /* Hide non-selectable years on mobile */
          .timeline-years label:not(.selectable) {
            display: none;
          }
        }
        
        /* Scale effect on hover */
        .timeline-years label.selectable:hover {
          transform: translateY(-2px);
        }
        
        /* Title styling */
        .timeline-title {
          font-weight: 600;
          margin-bottom: 5px;
          color: #212529;
        }
        
        /* Custom label styling - positioned at top right */
        .timeline-label-container {
          display: flex;
          justify-content: flex-end;
          align-items: center;
          margin-bottom: 5px;
        }
        
        .timeline-label {
          font-weight: bold;
          display: flex;
          align-items: center;
          color: #495057;
        }
        
        .timeline-label i {
          margin-right: 5px;
          color: #0275d8;
        }
      </style>
      <div class="timeline-label-container">
        <span class="timeline-label">
          <i class="far fa-calendar-days" role="presentation" aria-label="calendar-days icon"></i>
          ', label, '
        </span>
      </div>
      <div class="timeline-container">
        <div id="', inputId, '-timeline" class="timeline-years"></div>
      </div>
    </div>
    <script>
      // Wait for DOM to be ready
      $(document).ready(function() {
        // Initialize the timeline
        initTimeline();
        
        // Also initialize after Shiny is connected
        $(document).on("shiny:connected", function() {
          initTimeline();
        });
        
        function initTimeline() {
          // Full range of years (for continuous timeline)
          const fullRange = ', fullRangeJSON, ';
          
          // Years that are selectable
          const selectableYears = ', selectableYearsJSON, ';
          
          const timelineContainer = document.getElementById("', inputId, '-timeline");
          const inputId = "', inputId, '";
          let selectedValue = ', selectedJSON, ';
          
          // Clear existing content (in case this is reinitializing)
          if (timelineContainer) {
            timelineContainer.innerHTML = "";
            
            // Create elements for each year in the full range
            fullRange.forEach((year, index) => {
              const yearId = `${inputId}-${year}`;
              const isSelectable = selectableYears.includes(year.toString());
              
              // Create elements for selectable years
              if (isSelectable) {
                // Create input element (radio button)
                const input = document.createElement("input");
                input.type = "radio";
                input.id = yearId;
                input.name = inputId;
                input.value = year;
                
                // Check if this year matches the selected value
                if (year.toString() === selectedValue.toString()) {
                  input.checked = true;
                  console.log("Setting checked for: " + year);
                }
                
                // Create label
                const label = document.createElement("label");
                label.htmlFor = yearId;
                label.className = "selectable";
                
                // Create marker div (vertical line)
                const marker = document.createElement("div");
                marker.className = "year-marker";
                
                // Create year text (in circle)
                const yearText = document.createElement("div");
                yearText.className = "year-text";
                yearText.textContent = year;
                
                // Append elements
                label.appendChild(marker);
                label.appendChild(yearText);
                
                timelineContainer.appendChild(input);
                timelineContainer.appendChild(label);
                
                    // Add Shiny binding for the radio button
                input.addEventListener("change", function() {
                  // Send value to Shiny
                  Shiny.setInputValue(inputId, this.value);
                });
              } 
              // Create simpler markers for non-selectable years
              else {
                // Create a simple marker with just a line
                const label = document.createElement("label");
                
                // Create marker div (vertical line)
                const marker = document.createElement("div");
                marker.className = "year-marker";
                
                // Create a simple text label
                const yearLabel = document.createElement("div");
                yearLabel.className = "year-label";
                yearLabel.textContent = year;
              
                // Append elements
                label.appendChild(marker);
                label.appendChild(yearLabel);
                
                timelineContainer.appendChild(label);
              }
            });
            
            // Fix for NULL value: Always set the initial value
            // Make sure this runs immediately after creating the elements
            if (window.Shiny) {
              // Force set the input value
              setTimeout(function() {
                Shiny.setInputValue(inputId, selectedValue);
                console.log("Explicitly setting initial value: " + selectedValue);
              }, 100);
            }
            
            // Add animation when the component is loaded
            const timeline = document.querySelector(".timeline-container");
            if (timeline) {
              timeline.style.opacity = "0";
              timeline.style.transform = "translateY(10px)";
              timeline.style.transition = "opacity 0.5s ease, transform 0.5s ease";
              
              setTimeout(() => {
                timeline.style.opacity = "1";
                timeline.style.transform = "translateY(0)";
              }, 200);
            }
          }
        }
      });
    </script>'))
  
  # Return the HTML as a Shiny tag
  return(html)
}