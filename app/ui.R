# ui.R

my_theme <- bs_theme(bg = "#FBFCFE", fg = "#212529", primary = "#2B5C8F", base_font = font_google("Inter"))

custom_css_js <- tagList(
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap", rel = "stylesheet"),
    tags$style(HTML("
      /* - GLOBAL SCROLL - */
      html { scroll-padding-top: 120px; }
      
      /* - TITLE FONT - */
      h1, h2, h3, h4, h5, .navbar-nav .nav-link { font-family: 'Inter', sans-serif !important; }

      /* - INITIAL LOADING SCREEN - */
      #app-loader { position: fixed; width: 100%; height: 100%; background-color: #f1f4f8; z-index: 99999; top: 0; left: 0; display: flex; flex-direction: column; justify-content: center; align-items: center; transition: opacity 0.6s cubic-bezier(0.8, 0, 0.2, 1); }
      
      .modern-spinner { display: flex; justify-content: center; align-items: center; gap: 12px; margin-bottom: 25px; }
      .modern-spinner div { width: 18px; height: 18px; background-color: #2B5C8F; border-radius: 50%; animation: dot-bounce 1.4s infinite ease-in-out both; }
      .modern-spinner div:nth-child(1) { animation-delay: -0.32s; }
      .modern-spinner div:nth-child(2) { animation-delay: -0.16s; background-color: #4A7BB0; }
      .modern-spinner div:nth-child(3) { animation-delay: 0s; background-color: #73A1D4; }
      
      @keyframes dot-bounce { 
        0%, 80%, 100% { transform: scale(0); opacity: 0.3; } 
        40% { transform: scale(1); opacity: 1; } 
      }
      
      @keyframes loader-text-pulse { 0%, 100% { opacity: 0.5; } 50% { opacity: 1; } }
      .loader-text { font-size: 16px; font-weight: 700; color: #2B5C8F; letter-spacing: 2px; font-family: 'Inter', sans-serif; animation: loader-text-pulse 2s infinite ease-in-out; }

      /* - BACKGROUND - */
      body { background: linear-gradient(135deg, #f1f4f8, #edf1f6, #e9edf3); background-size: 200% 200%; animation: subtleGradient 15s ease infinite; padding-top: 100px !important; display: flex; flex-direction: column; min-height: 100vh; overflow-x: hidden; }
      @keyframes subtleGradient { 0% { background-position: 0% 50%; } 50% { background-position: 100% 50%; } 100% { background-position: 0% 50%; } }
      
      /* - CUSTOM DRAG & DROP FILE INPUT - */
      .file-drop-zone { margin-bottom: 10px; width: 100%; }
      .file-drop-zone .input-group, .file-drop-zone .input-group-btn, .file-drop-zone .input-group-prepend { display: block !important; width: 100% !important; }
      .file-drop-zone .form-control { display: none !important; } 
      .file-drop-zone .btn-file { display: block; width: 100%; border: 2px dashed #A4C2E4 !important; border-radius: 12px !important; background: linear-gradient(to bottom, #ffffff, #f8fafd) !important; padding: 30px 20px 20px 20px !important; text-align: center !important; transition: all 0.3s ease !important; color: #212529 !important; white-space: normal; box-shadow: inset 0 2px 5px rgba(0,0,0,0.02); }
      .file-drop-zone .btn-file:hover { border-color: #2B5C8F !important; background: linear-gradient(to bottom, #f8fafd, #eef4fa) !important; box-shadow: 0 4px 10px rgba(43, 92, 143, 0.08) !important; transform: translateY(-2px) !important; }
      .file-drop-zone .fa-cloud-arrow-up { font-size: 2.8rem; color: #73A1D4; margin-bottom: 12px; display: block; transition: transform 0.3s ease; }
      .file-drop-zone .btn-file:hover .fa-cloud-arrow-up { transform: scale(1.1); color: #2B5C8F; }
      .file-drop-zone .drop-text { font-weight: 700; color: #212529; font-size: 1.1rem; display: block; margin-top: 5px; }

      /* - HOME PAGE - */
      .tab-pane, .tab-pane > .container-fluid { padding-top: 0 !important; }
      .hero-section-wrapper { 
        background: linear-gradient(to right, #7634b3, #52a3c2) !important; 
        width: 100vw; 
        position: relative; 
        left: 50%; 
        transform: translateX(-50%); 
        padding: 120px 0 80px 0 !important; /* Extra padding top to avoid hiding under navbar */
        margin-top: -100px !important; /* Pulls up EXACTLY the 100px body padding */
        border-bottom: 1px solid #d0e0ef; 
      }
      .faq-section-wrapper { background-color: #ffffff; width: 100vw; position: relative; left: 50%; transform: translateX(-50%); padding: 60px 0 80px 0; box-shadow: 0 -4px 20px rgba(0,0,0,0.02); }

      /* - LAYOUT - */
      #main_panel { transition: width 0.4s cubic-bezier(0.4, 0, 0.2, 1); }
      #main_panel.col-sm-12 { max-width: 100%; flex: 0 0 100%; }
      #results_tabs_ui { width: 100%; }
      #results_tabs_ui > div { width: 100%; }

      .glass-panel { background: rgba(255, 255, 255, 0.85) !important; backdrop-filter: blur(10px); -webkit-backdrop-filter: blur(10px); border: 1px solid rgba(255, 255, 255, 0.4) !important; box-shadow: 0 8px 32px 0 rgba(31, 38, 135, 0.05) !important; border-radius: 12px !important; }

      /* - GLOW BUTTON FOR SIDEBAR - */
      .btn-run-glow { transition: all 0.3s ease; border-radius: 40px; position: relative; z-index: 1; }
      .btn-run-glow::before, .btn-run-glow::after { content: ''; position: absolute; top: 0; left: 0; right: 0; bottom: 0; border-radius: inherit; box-shadow: 0 0 0 0 rgba(43, 92, 143, 0.7); z-index: -1; }
      .btn-run-glow::before { animation: pulse-ring-heavy 2.5s infinite cubic-bezier(0.215, 0.61, 0.355, 1); }
      .btn-run-glow::after { animation: pulse-ring-heavy 2.5s infinite cubic-bezier(0.215, 0.61, 0.355, 1) 1.25s; }
      
      /* - CTA BUTTON - */
      .btn-hero-cta { transition: all 0.3s ease; border-radius: 40px; position: relative; z-index: 1; background-color: #FF9800 !important; border-color: #FF9800 !important; color: white !important; box-shadow: 0 4px 15px rgba(255, 152, 0, 0.3); animation: cta-pulse 2s infinite ease-in-out; }
      .btn-hero-cta:hover { transform: translateY(-2px); background-color: #F57C00 !important; border-color: #F57C00 !important; color: white !important; box-shadow: 0 8px 25px rgba(255, 152, 0, 0.5); animation: none; }
      @keyframes cta-pulse { 0% { transform: scale(1); box-shadow: 0 4px 15px rgba(255, 152, 0, 0.3); } 50% { transform: scale(1.04); box-shadow: 0 4px 25px rgba(255, 152, 0, 0.6); } 100% { transform: scale(1); box-shadow: 0 4px 15px rgba(255, 152, 0, 0.3); } }
      @keyframes pulse-ring-heavy { 0% { transform: scale(0.95); box-shadow: 0 0 0 0 rgba(43, 92, 143, 0.8); } 70% { transform: scale(1); box-shadow: 0 0 0 25px rgba(43, 92, 143, 0); } 100% { transform: scale(0.95); box-shadow: 0 0 0 0 rgba(43, 92, 143, 0); } }
      .btn-run-glow:hover { transform: translateY(-2px); background-color: #4A7BB0 !important; border-color: #4A7BB0 !important; color: white !important; box-shadow: 0 8px 25px rgba(43, 92, 143, 0.5); }
      .btn-run-glow:hover::before, .btn-run-glow:hover::after { animation: none; }
      .btn-run-glow:disabled, .btn-run-glow.disabled { opacity: 0.65; cursor: not-allowed; transform: none !important; }
      .btn-run-glow:disabled::before, .btn-run-glow:disabled::after { animation: none; }
      
      /* - BUTTON PULSE - */
      .btn-reset-pulse { background-color: #eef4fa !important; color: #2B5C8F !important; border: 2px solid #73A1D4 !important; transition: all 0.3s ease; animation: gentle-pulse 3s infinite ease-in-out; }
      .btn-reset-pulse:hover { background-color: #73A1D4 !important; color: white !important; animation: none; transform: translateY(-2px); box-shadow: 0 6px 15px rgba(43, 92, 143, 0.2); }
      @keyframes gentle-pulse { 0% { box-shadow: 0 0 0 0 rgba(115, 161, 212, 0.5); } 50% { box-shadow: 0 0 0 10px rgba(115, 161, 212, 0); } 100% { box-shadow: 0 0 0 0 rgba(115, 161, 212, 0); } }

      /* - DOC SIDEBAR LINKS - */
      .doc-sidebar-link { display: block; padding: 10px 15px; margin-bottom: 5px; color: #4A4A4A !important; text-decoration: none !important; font-weight: 500; font-size: 15px; border-radius: 8px; transition: all 0.2s ease; border-left: 3px solid transparent; }
      .doc-sidebar-link:hover { background-color: #eef4fa; color: #2B5C8F !important; border-left: 3px solid #2B5C8F; transform: translateX(3px); }

      /* - HOVER REVEAL CARDS FOR TUTORIAL DATA - */
      .tutorial-card-link { display: block; text-decoration: none !important; color: inherit !important; }
      .tutorial-card-link:hover { color: inherit !important; }
      .tutorial-card { background: white; border: 1px solid #E5E4E2; border-radius: 8px; padding: 18px 20px; margin-bottom: 12px; transition: all 0.3s ease; position: relative; overflow: hidden; }
      .tutorial-card:hover { border-color: #2B5C8F; transform: translateY(-2px); box-shadow: 0 6px 15px rgba(43, 92, 143, 0.12); }
      .dl-overlay { position: absolute; top: 0; left: 0; right: 0; bottom: 0; background: rgba(155, 192, 229, 0.85); color: white; display: flex; align-items: center; justify-content: center; opacity: 0; transition: opacity 0.25s ease; }
      .tutorial-card:hover .dl-overlay { opacity: 1; }
      .dl-overlay i { font-size: 2.2rem; transition: transform 0.3s cubic-bezier(0.175, 0.885, 0.32, 1.275); transform: scale(0.5); }
      .tutorial-card:hover .dl-overlay i { transform: scale(1); }

      @keyframes badge-pulse { 0% { transform: scale(1); box-shadow: 0 0 0 0 rgba(255, 255, 255, 0.4); } 50% { transform: scale(1.05); box-shadow: 0 0 0 10px rgba(255, 255, 255, 0); } 100% { transform: scale(1); box-shadow: 0 0 0 0 rgba(255, 255, 255, 0); } }
      .available-badge { display: inline-block; background-color: rgba(255,255,255,0.15); color: #ffffff !important; font-weight: 700; padding: 6px 16px; border-radius: 20px; font-size: 1.0rem; border: 1px solid rgba(255,255,255,0.3); font-family: 'Inter', sans-serif; animation: badge-pulse 2s infinite ease-in-out; }

      .accordion-item { border: none !important; background-color: transparent !important; margin-bottom: 15px; }
      .accordion-button { font-weight: 600 !important; color: #212529 !important; background-color: rgba(255,255,255,0.9) !important; border: 1px solid #E5E4E2 !important; border-radius: 12px !important; transition: all 0.3s ease; box-shadow: 0px 4px 10px rgba(0,0,0,0.03); }
      .accordion-button:hover { background-color: #f4f8fc !important; border-color: #73A1D4 !important; transform: translateY(-1px); box-shadow: 0px 6px 15px rgba(43, 92, 143, 0.1); }
      .accordion-button:not(.collapsed) { color: #2B5C8F !important; background-color: #f4f8fc !important; border-color: #2B5C8F !important; box-shadow: 0px 4px 15px rgba(43, 92, 143, 0.12) !important; border-bottom-left-radius: 0 !important; border-bottom-right-radius: 0 !important; }
      .accordion-body { background-color: rgba(255,255,255,0.95); border: 1px solid #2B5C8F; border-top: none; border-radius: 0 0 12px 12px; padding: 20px; color: #4A4A4A; line-height: 1.6; box-shadow: 0px 8px 15px rgba(43, 92, 143, 0.06); }
      
      @keyframes softPulse { 0% { transform: scale(1); opacity: 1; } 50% { transform: scale(1.03); opacity: 0.8; text-shadow: 0px 4px 10px rgba(43, 92, 143, 0.3); } 100% { transform: scale(1); opacity: 1; } }
      .pulse-text { animation: softPulse 2.5s infinite ease-in-out; display: inline-block; }

      /* - BACKGROUND OVERLAY - */
      #progress_overlay {
        position: fixed; top: 0; left: 0; right: 0; bottom: 0;
        background: rgba(33, 37, 41, 0.6);
        backdrop-filter: blur(3px); -webkit-backdrop-filter: blur(3px);
        z-index: 10040; display: none;
      }
      
      /* - GITHUB ICON - */
      .github-footer-icon {
        color: #2B5C8F !important;
        font-size: 45px;
        transition: all 0.3s cubic-bezier(0.175, 0.885, 0.32, 1.275);
        filter: drop-shadow(0 4px 6px rgba(43, 92, 143, 0.15));
      }
      .github-footer-icon:hover {
        color: #1a3c5e !important;
        transform: scale(1.15) rotate(-5deg);
        filter: drop-shadow(0 8px 12px rgba(43, 92, 143, 0.3));
      }
      
      .github-btn {
        background-color: #24292e !important;
        color: white !important;
        transition: all 0.3s ease;
        border-radius: 30px;
        box-shadow: 0 4px 15px rgba(36, 41, 46, 0.3);
      }
      .github-btn:hover {
        background-color: #000000 !important;
        transform: translateY(-3px);
        box-shadow: 0 8px 25px rgba(36, 41, 46, 0.5);
      }

      /* - PROGRESS BAR - */
      @keyframes custom-progress-stripes { from { background-position: 1.2rem 0; } to { background-position: 0 0; } }
      #shiny-notification-panel {
        position: fixed !important; top: 0 !important; left: 0 !important; right: 0 !important; bottom: 0 !important; width: 100vw !important; height: 100vh !important; pointer-events: none !important; z-index: 10050 !important; display: flex !important; flex-direction: column !important; justify-content: flex-end !important; align-items: flex-end !important; padding: 24px !important;
      }
      .shiny-notification:has(.progress-message), .shiny-progress-container {
        position: fixed !important; top: 50% !important; left: 50% !important; transform: translate(-50%, -50%) !important; right: auto !important; bottom: auto !important; width: 420px !important; max-width: 90vw !important; background-color: #ffffff !important; border-radius: 16px !important; border: 1px solid rgba(43,92,143,0.15) !important; box-shadow: 0 20px 60px rgba(43,92,143,0.18), 0 4px 12px rgba(0,0,0,0.06) !important; padding: 32px 32px 28px 32px !important; text-align: center !important; pointer-events: auto !important;
      }
      .shiny-notification:has(.progress-message) .shiny-notification-close { display: none !important; }
      .shiny-notification .progress-text { position: static !important; background: transparent !important; border: none !important; box-shadow: none !important; padding: 0 !important; margin-bottom: 20px !important; width: 100% !important; opacity: 1 !important; }
      .shiny-notification .progress-message { font-family: 'Inter', sans-serif !important; font-weight: 800 !important; font-size: 1.4rem !important; color: #2B5C8F !important; display: block !important; margin-bottom: 8px !important; letter-spacing: 0.3px !important; }
      .shiny-notification .progress-detail { font-family: 'Inter', sans-serif !important; font-size: 1rem !important; color: #6c757d !important; display: block !important; margin: 0 !important; font-style: normal !important; }
      .shiny-notification .progress { background-color: #eef4fa !important; border-radius: 10px !important; height: 16px !important; margin: 0 !important; box-shadow: inset 0 2px 4px rgba(0,0,0,0.06) !important; overflow: hidden !important; }
      .shiny-notification .progress-bar { background-color: #2B5C8F !important; background-image: linear-gradient(45deg, rgba(255,255,255,0.25) 25%, transparent 25%, transparent 50%, rgba(255,255,255,0.25) 50%, rgba(255,255,255,0.25) 75%, transparent 75%, transparent) !important; background-size: 1.2rem 1.2rem !important; animation: custom-progress-stripes 1s linear infinite !important; border-radius: 10px !important; transition: width 0.3s ease !important; }
      .shiny-notification:has(.progress-message)::after { content: 'Please wait...'; display: block !important; font-style: italic !important; font-weight: 500 !important; color: #6c757d !important; font-size: 0.95rem !important; margin-top: 20px !important; animation: softPulse 2.5s infinite ease-in-out !important; }
       
       /* - NOTIFICATIONS - */
      
      .shiny-notification:not(:has(.progress-message)) { position: relative !important; margin: 10px 0 0 0 !important; width: 380px !important; max-width: calc(100vw - 48px) !important; padding: 16px 20px !important; border-radius: 12px !important; border: none !important; font-family: 'Inter', sans-serif !important; font-size: 0.92rem !important; line-height: 1.5 !important; color: #212529 !important; box-shadow: 0 8px 30px rgba(0,0,0,0.12), 0 2px 8px rgba(0,0,0,0.06) !important; animation: notif-slide-in 0.35s cubic-bezier(0.22, 1, 0.36, 1) !important; opacity: 1 !important; background: #ffffff !important; pointer-events: auto !important; text-align: left !important; }
      @keyframes notif-slide-in { from { transform: translateX(30px); opacity: 0; } to { transform: translateX(0); opacity: 1; } }
      .shiny-notification-error:not(:has(.progress-message)) { border-left: 5px solid #DC3545 !important; background: linear-gradient(to right, #fff5f5, #ffffff) !important; }
      .shiny-notification-error:not(:has(.progress-message)) .shiny-notification-close { color: #DC3545 !important; font-size: 1.1rem !important; opacity: 0.7; transition: opacity 0.2s; display: block !important; }
      .shiny-notification-error:not(:has(.progress-message)) .shiny-notification-close:hover { opacity: 1; }
      .shiny-notification-warning:not(:has(.progress-message)) { border-left: 5px solid #F59E0B !important; background: linear-gradient(to right, #FFFBEB, #ffffff) !important; }
      .shiny-notification-message:not(:has(.progress-message)) { border-left: 5px solid #2B5C8F !important; background: linear-gradient(to right, #eef4fa, #ffffff) !important; }
      .shiny-notification:not(.shiny-notification-error):not(:has(.progress-message)) .shiny-notification-close { display: none !important; }

      /* - FOOTER - */
      .bslib-page-navbar > .tab-content { display: flex; flex-direction: column; flex-grow: 1; }
      .tab-pane.active { display: flex !important; flex-direction: column !important; flex-grow: 1; min-height: calc(100vh - 100px); }
      .tab-pane > .container-fluid { flex-grow: 1; display: flex; flex-direction: column; }
      .ces-footer { margin-top: auto !important; flex-shrink: 0; box-sizing: border-box; width: 100vw; position: relative; left: 50%; transform: translateX(-50%); background: linear-gradient(to right, #f4eaff, #e5f4f9) !important; border-top: 1px solid #d0e0ef; }

      /* - NAVBAR - */
      :root { --bs-navbar-border-width: 0px !important; --bs-navbar-box-shadow: none !important; --bs-border-width: 0px !important; }
      nav.navbar, .bslib-page-navbar > nav.navbar, header.navbar, .navbar.border-bottom { border: none !important; border-bottom: 0px solid transparent !important; box-shadow: 0 4px 15px rgba(0,0,0,0.1) !important; -webkit-box-shadow: 0 4px 15px rgba(0,0,0,0.1) !important; background: linear-gradient(to right, #7634b3, #52a3c2) !important; position: fixed !important; top: 0 !important; width: 100% !important; z-index: 1030 !important; }
      .navbar::after, .navbar::before, .bslib-page-navbar > .navbar::after, .bslib-page-navbar > .navbar::before { display: none !important; content: none !important; }
      .bslib-page-navbar .tab-content, .bslib-navs-container, .nav-tabs { border-top: none !important; border-bottom: none !important; box-shadow: none !important; }

      /* - NAVBAR BRAND & LINKS - */
      .logo-link { cursor: pointer; display: flex; align-items: center; text-decoration: none; }
      .logo-link:hover { opacity: 0.8; transition: opacity 0.2s; }
      .navbar-nav .nav-link { font-size: 20px !important; font-weight: 500; color: rgba(255, 255, 255, 0.75) !important; padding: 10px 20px !important; margin: 0 5px; border-bottom: 3px solid transparent; transition: all 0.2s ease; display: flex; align-items: center; gap: 8px; }
      .navbar-nav .nav-link:hover { color: #ffffff !important; background-color: rgba(255, 255, 255, 0.1); border-radius: 8px 8px 0 0; }
      .navbar-nav .nav-link.active { color: #ffffff !important; font-weight: 700; border-bottom: 3px solid #ffffff !important; background-color: transparent; }
      .navbar-brand { padding: 0 !important; margin: 0 !important; display: flex !important; align-items: center !important; }

      .shiny-output-error { color: transparent !important; }
      .shiny-output-error:before { content: 'An unexpected visual error occurred. Please verify your data format.'; color: #2B5C8F; font-weight: bold; font-size: 14px; display: block; padding: 10px; background: rgba(43, 92, 143, 0.08); border-radius: 8px; border-left: 4px solid #2B5C8F; }

      .hero-image-container { width: 100%; display: flex; justify-content: center; }
      .hero-image-container img { width: 100%; max-width: 500px; height: auto; object-fit: contain; }
      @media (max-width: 991px) { .hero-text-col { text-align: center !important; margin-top: 30px !important; padding: 0 15px !important; } }

      .dataTables_wrapper .dataTables_paginate .paginate_button { color: #73A1D4 !important; font-weight: 500; }
      .dataTables_wrapper .dataTables_paginate .paginate_button.current { background: #73A1D4 !important; color: white !important; border: none !important; border-radius: 6px; box-shadow: 0 2px 5px rgba(115, 161, 212, 0.3); }
      .dataTables_wrapper .dataTables_paginate .paginate_button:hover { background: #9bc0e5 !important; color: white !important; border: none !important; border-radius: 6px; }
      table.dataTable.hover tbody tr:hover, table.dataTable.display tbody tr:hover { background-color: rgba(43, 92, 143, 0.06) !important; transition: background-color 0.2s;}

      .selectize-input, .form-control { border-radius: 8px !important; border: 1px solid #ced4da !important; box-shadow: inset 0 1px 2px rgba(0,0,0,0.05) !important; transition: all 0.3s ease; padding: 10px; }
      .selectize-input.focus, .form-control:focus { border-color: #4A7BB0 !important; box-shadow: 0 0 0 0.2rem rgba(43, 92, 143, 0.25) !important; outline: none;}

      .results-tabs > .nav-tabs { border-bottom: 2px solid #E5E4E2 !important; margin-bottom: 25px; } 
      .results-tabs > .nav-tabs .nav-link { border: none !important; color: #6c757d !important; font-weight: 600; padding: 12px 25px; transition: all 0.3s ease; background: transparent; font-size: 16px;}
      .results-tabs > .nav-tabs .nav-link:hover { color: #2B5C8F !important; background: rgba(43, 92, 143, 0.08); border-radius: 8px 8px 0 0; }
      .results-tabs > .nav-tabs .nav-link.active { color: #2B5C8F !important; border-bottom: 3px solid #2B5C8F !important; background: transparent; }

      .irs--shiny .irs-bar { background: #2B5C8F !important; border-top: 1px solid #2B5C8F !important; border-bottom: 1px solid #2B5C8F !important; }
      .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single { background: #2B5C8F !important; }
      .irs--shiny .irs-handle { border: 1px solid #2B5C8F !important; }
      .irs--shiny .irs-handle:hover { border-color: #4A7BB0 !important; }
      .control-label { font-weight: 600; color: #212529; margin-bottom: 6px; }
      
      #backToTop { display: none; position: fixed; bottom: 30px; right: 30px; z-index: 1000; border: none; outline: none; background-color: #2B5C8F; color: white; cursor: pointer; padding: 12px 18px; border-radius: 50%; font-size: 20px; box-shadow: 0px 4px 15px rgba(43, 92, 143, 0.4); transition: all 0.3s ease; }
      #backToTop:hover { background-color: #4A7BB0; transform: scale(1.1); }
      
      /* - RADIO BUTTON - */
      .shiny-options-group .radio input[type='radio'] {
        accent-color: #2B5C8F; 
        cursor: pointer;
      }
      .shiny-options-group .radio label {
        cursor: pointer;
      }

      /* - TOXICITY SLIDER - */
      .irs--shiny .irs-line { background: #e9ecef !important; border: 1px solid #ced4da !important; border-radius: 10px !important; height: 8px !important; }
      .irs--shiny .irs-bar { background: linear-gradient(to right, #73A1D4, #2B5C8F) !important; border: none !important; height: 8px !important; box-shadow: 0 2px 5px rgba(43, 92, 143, 0.3); }
      .irs--shiny .irs-handle { top: 17px !important; width: 24px !important; height: 24px !important; border: 3px solid #2B5C8F !important; background: #ffffff !important; box-shadow: 0 3px 8px rgba(0,0,0,0.2) !important; border-radius: 50% !important; transition: transform 0.2s ease, box-shadow 0.2s ease; cursor: grab !important; }
      .irs--shiny .irs-handle:hover { transform: scale(1.15); box-shadow: 0 6px 15px rgba(43, 92, 143, 0.4) !important; background: #f4f8fc !important; border-color: #4A7BB0 !important; }
      .irs--shiny .irs-handle:active { cursor: grabbing !important; transform: scale(1.05); }
      .irs--shiny .irs-single { background: #2B5C8F !important; color: white !important; font-weight: 700 !important; font-size: 13px !important; border-radius: 6px !important; padding: 4px 10px !important; box-shadow: 0 4px 10px rgba(43, 92, 143, 0.2); }
      .irs--shiny .irs-single::before { border-top-color: #2B5C8F !important; }

    ")),
    tags$script(HTML("
      window.onscroll = function() {scrollFunction()};
      function scrollFunction() { document.getElementById('backToTop').style.display = (document.body.scrollTop > 150 || document.documentElement.scrollTop > 150) ? 'block' : 'none'; }
      function topFunction() { window.scrollTo({top: 0, behavior: 'smooth'}); }
    "))
  )
)

simple_footer <- div(
  class = "ces-footer",
  style = "padding: 30px 20px; display: flex; align-items: center; justify-content: center; gap: 35px; flex-wrap: wrap;",
  
  a(href = "https://www.helsinki.fi/en/hilife-helsinki-institute-life-science/units/fimm", target = "_blank", class = "logo-link",
    img(src = "FIMM_logo.png", style = "height: 65px; object-fit: contain;")
  ),
  
  a(href = "https://www.helsinki.fi/en/researchgroups/hematology-research-unit-helsinki", target = "_blank", class = "logo-link",
    img(src = "HRUH_logo.png", style = "height: 65px; object-fit: contain;")
  ),
  
  a(href = "https://www.helsinki.fi/en", target = "_blank", class = "logo-link",
    img(src = "HY_logo.png", style = "height: 65px; object-fit: contain;")
  ),
  
  a(href = "https://github.com/dias-dio/CES", target = "_blank", class = "logo-link github-footer-icon", 
    icon("github")
  ),
  
  p("\u00a9 Copyright 2026 CES. All rights reserved.", style = "color: #4A4A4A; font-weight: 500; font-size: 14px; margin: 0;")
)

ui <- tagList(
  div(id = "app-loader",
      div(class = "modern-spinner",
          div(), div(), div()
      ),
      div(class = "loader-text", "Loading CES pipeline...")
  ),
  
  div(id = "progress_overlay"), 
  
  tags$button(id = "backToTop", onclick = "topFunction()", icon("arrow-up")),
  
  page_navbar(
    theme = my_theme, 
    title = actionLink("logo_home_link", label = img(src = "app_logo.svg", style = "height: 70px; width: auto; margin-left: 300px; margin-right: 15px; filter: brightness(0) invert(1);"), class = "logo-link"),
    window_title = "CES Framework", id = "main_navbar", fillable = FALSE,
    
    header = tagList(useShinyjs(), custom_css_js), 
    
    nav_spacer(), 
    
    # Home page
    nav_panel(title = "Home", value = "Home",
              fluidPage(
                div(class = "hero-section-wrapper",
                    div(style = "max-width: 1350px; margin: 0 auto; padding: 0 40px;",
                        fluidRow(
                          style = "margin-bottom: 25px; position: relative; z-index: 10;",
                          column(12, align = "right",
                                 span(class = "available-badge", "Available now")
                          )
                        ),
                        fluidRow(
                          style = "display: flex; align-items: center; justify-content: center;",
                          column(12, align = "center", class = "hero-text-col", style = "padding: 0 20px;",
                                 h1("CES \u2013 Co-culture Efficacy Score", style = "font-weight: 800; color: #ffffff; margin-bottom: 12px; font-size: 3.2rem; text-shadow: 0px 2px 4px rgba(0,0,0,0.2);"),
                                 h3("A scoring platform for co-culture screening data", style = "font-weight: 600; color: rgba(255,255,255,0.95); margin-bottom: 22px; font-size: 1.8rem; text-shadow: 0px 1px 3px rgba(0,0,0,0.1);"),
                                 p("Rapidly quantify and prioritize compound activity in multicellular systems. CES distinguishes effector-mediated modulation from general toxicity, supporting applications across cancer immunotherapy and host-pathogen antiviral screens.", style = "font-size: 1.25rem; color: rgba(255,255,255,0.85); margin-bottom: 35px; line-height: 1.65; max-width: 900px; margin-left: auto; margin-right: auto;"),
                                 actionButton("go_to_analysis", tagList("Go to analysis ", icon("arrow-right")), class = "btn-hero-cta", style = "margin-top: 5px; font-size: 1.15rem; font-weight: bold; padding: 14px 40px;")
                          )
                        )
                    )
                ),
                div(class = "faq-section-wrapper",
                    div(style = "max-width: 1350px; margin: 0 auto; padding: 0 40px;",
                        fluidRow(
                          style = "display: flex; align-items: flex-start;", 
                          column(6, style = "padding-right: 50px;",
                                 div(class = "glass-panel", style = "padding: 40px; border-radius: 12px;",
                                     h3("FAQ", class = "pulse-text", style = "color: #2B5C8F; font-weight: bold; font-size: 2rem; margin-bottom: 10px;"),
                                     h4("General questions", style = "color: #4A4A4A; margin-bottom: 25px; font-weight: 500;"),
                                     accordion(
                                       id = "faq_accordion", open = FALSE, 
                                       accordion_panel(
                                         title = tagList(icon("circle-question", class = "fa-solid fa-fw", style = "color: #73A1D4; font-size: 2rem; vertical-align: -0.1em; margin-right: 8px;"), "What is the Co-culture Efficacy Score (CES)?"), 
                                         value = "faq1",
                                         tagList(
                                           p("The Co-culture Efficacy Score (CES) is a quantitative framework for measuring drug activity in complex multicellular systems. It integrates cumulative activity and maximal efficacy into a single interpretable score, separating general toxicity from true effector-mediated cytotoxicity."),
                                           p("For a complete breakdown of all pipeline output metrics (including nAUC, Peak, and toxicity flags), refer to the ",
                                             actionLink("link_to_about", "Documentation page", style = "color: #2B5C8F; text-decoration: underline; font-weight: bold;")
                                           )
                                         )
                                       ),
                                       
                                       accordion_panel(
                                         title = tagList(icon("circle-question", class = "fa-solid fa-fw", style = "color: #73A1D4; font-size: 2rem; vertical-align: -0.1em; margin-right: 8px;"), "What is the difference between the Therapeutic and Mechanistic models?"), 
                                         value = "faq2",
                                         tagList(
                                           p(strong("Therapeutic CES: "), "This model quantifies overall co-culture drug efficacy. It favors compounds that maximize target cell elimination while actively penalizing compounds that compromise effector cell viability, thereby reflecting therapeutic potential in co-culture environments."),
                                           p(strong("Mechanistic CES: "), "This model isolates immunomodulation by adjusting for compound toxicity against effector cells. It captures drug activity that either enhances or inhibits effector-mediated cytotoxicity by mathematically filtering out artifacts caused by toxicity to the effector cell population."),
                                           p(em("Applicable to 3-condition setups (co-culture, target monoculture, and effector monoculture)."), 
                                             style = "color: #6c757d; font-size: 0.88rem; margin-top: 12px; margin-bottom: 0;")
                                         )
                                       ),
                                       
                                       accordion_panel(
                                         title = tagList(icon("circle-question", class = "fa-solid fa-fw", style = "color: #73A1D4; font-size: 2rem; vertical-align: -0.1em; margin-right: 8px;"), "How do I format and upload my data?"), 
                                         value = "faq3",
                                         tagList(
                                           "To upload your co-culture screening data successfully, please format your file according to the CES input requirements. The ", 
                                           actionLink("link_to_docs_data_req", "Documentation page", style = "color: #2B5C8F; text-decoration: underline; font-weight: bold;"), 
                                           " provides detailed guidance on the required columns, file structure, and example input files."
                                         )
                                       ),
                                       
                                       accordion_panel(
                                         title = tagList(icon("circle-question", class = "fa-solid fa-fw", style = "color: #73A1D4; font-size: 2rem; vertical-align: -0.1em; margin-right: 8px;"), "How should I cite CES?"), 
                                         value = "faq4",
                                         tagList(
                                           "A manuscript is currently in preparation. Please see the ", 
                                           actionLink("link_to_cite", "Cite page", style = "color: #2B5C8F; text-decoration: underline; font-weight: bold;"), 
                                           " for updates."
                                         )
                                       )
                                     )
                                 )
                          ),
                          column(6, align = "center", style = "padding-left: 50px;",
                                 div(class = "hero-image-container", 
                                     img(src = "faq_logo.svg", style = "height: 500px; width: auto; max-width: 100%; object-fit: contain;")
                                 )
                          )
                        )
                    )
                )
              ),
              simple_footer
    ),
    
    # Analysis page
    nav_panel(title = "Analysis", value = "Analysis",
              fluidPage(
                fluidRow(style = "margin: 0;",
                         div(id = "sidebar_panel", class = "col-sm-3", style = "padding: 0 15px;",
                             div(class = "glass-panel", style = "padding: 25px;", 
                                 
                                 div(id = "analysis_sidebar_inputs",
                                     h4(icon("arrow-up-from-bracket"), " Data upload", style = "font-weight: bold; color: #212529; margin-bottom: 15px;"),
                                     
                                     div(class = "file-drop-zone",
                                         fileInput("file_upload", 
                                                   label = NULL, 
                                                   buttonLabel = tagList(
                                                     icon("cloud-arrow-up"), 
                                                     span("Choose a file or drag it here", class = "drop-text")
                                                   ), 
                                                   accept = c(".csv", ".xlsx", ".txt", ".tsv"), 
                                                   width = "100%")
                                     ),
                                     div(HTML("Supported formats: <b>.csv</b>, <b>.xlsx</b>, <b>.txt/.tsv</b><br>Maximum file size: 50MB"), 
                                         style = "font-size: 14px; color: #6c757d; margin-top: -5px; margin-bottom: 25px; line-height: 1.6; text-align: center;"),
                                     
                                     radioButtons("screening_type", tagList(icon("flask"), " Assay type"), choices = c("Drug sensitivity", "Antiviral")),
                                     
                                     conditionalPanel(
                                       condition = "input.screening_type == 'Drug sensitivity'",
                                       div(style = "margin-top: 15px;", radioButtons("data_format", tagList(icon("file-import"), " Data format"), choices = c("Annotation file", "Processed"), selected = "Annotation file"))
                                     ),
                                     
                                     conditionalPanel(
                                       condition = "input.screening_type == 'Drug sensitivity' && input.data_format == 'Processed'",
                                       div(style = "margin-top: 15px;", selectInput("readout_type", tagList(icon("percent"), " Readout"), choices = c("Viability (%)", "Inhibition (%)"), selected = "Inhibition (%)"))
                                     ),
                                     
                                     conditionalPanel(
                                       condition = "input.screening_type == 'Drug sensitivity' && output.has_control === true",
                                       div(style = "margin-top: 15px;", radioButtons("scoring_model", tagList(icon("network-wired"), " Scoring model (3-condition)"), choices = c("Therapeutic", "Mechanistic"), selected = "Therapeutic")),
                                       div(style = "font-size: 12px; color: #6c757d; margin-top: -10px; margin-bottom: 15px;",
                                           "The toxicity threshold (DSS) can be tuned after running the analysis.")
                                     )
                                 ),
                                 
                                 hr(style = "border-top: 1px solid #E5E4E2; margin-top: 10px; margin-bottom: 15px;"),
                                 div(style = "display: flex; justify-content: center; width: 100%;",
                                     actionButton("run_analysis", tagList(icon("bolt"), " Run analysis"),
                                                  class = "btn-hero-cta",
                                                  style = "width: 60%; font-size: 1.1rem; font-weight: bold; padding: 12px; margin-top: 5px;")
                                 ),
                             )
                         ),
                         div(id = "main_panel", class = "col-sm-9", style = "padding: 0 15px;",
                             conditionalPanel(
                               condition = "output.analysis_success !== true",
                               div(style = "max-width: 1150px; margin: 0 auto;",
                                   fluidRow(
                                     column(8,
                                            div(style = "background-color: white; padding: 40px; border-radius: 12px; box-shadow: 0 4px 20px rgba(0,0,0,0.05); border: 1px solid #E5E4E2;",
                                                h3("Welcome to the CES analysis pipeline!", style = "color: #212529; font-weight: 800; margin-bottom: 10px; font-size: 1.7rem;"),
                                                p("Upload your data using the sidebar menu. Review the format requirements below before running the analysis.", style = "color: #6c757d; font-size: 1.05rem; margin-bottom: 30px;"),
                                                
                                                conditionalPanel(
                                                  condition = "input.screening_type == 'Drug sensitivity' && input.data_format == 'Annotation file'",
                                                  div(style = "background-color: #f8f9fa; padding: 25px; border-radius: 10px; border-left: 5px solid #2B5C8F;",
                                                      h5(icon("flask-vial", class="fa-fw"), " Annotation file format", style = "color: #2B5C8F; font-weight: 700; margin-bottom: 15px;"),
                                                      p("For raw plate reader signals. Normalized percent inhibition is computed automatically from your internal plate controls.", style = "font-size: 0.95rem; color: #4A4A4A;"),
                                                      tags$ul(style = "font-size: 0.9rem; color: #4A4A4A; padding-left: 15px; line-height: 1.6;",
                                                              tags$li(strong("Required columns: "), "WELL, PLATE, DRUG, CONCENTRATION, SCREEN, WELL_SIGNAL."),
                                                              tags$li(strong("Control anchors: "), "The DRUG column must include ", strong("BZCL"), " (or POS) and ", strong("DMSO"), " (or NEG). Co-culture plates require ", strong("DMSO2"), " (or NEG2).")
                                                      )
                                                  )
                                                ),
                                                conditionalPanel(
                                                  condition = "input.screening_type == 'Drug sensitivity' && input.data_format == 'Processed'",
                                                  div(style = "background-color: #f8f9fa; padding: 25px; border-radius: 10px; border-left: 5px solid #2B5C8F;",
                                                      h5(icon("table", class="fa-fw"), " Processed data format", style = "color: #2B5C8F; font-weight: 700; margin-bottom: 15px;"),
                                                      p("For data already normalized to percent viability or inhibition.", style = "font-size: 0.95rem; color: #4A4A4A;"),
                                                      tags$ul(style = "font-size: 0.9rem; color: #4A4A4A; padding-left: 15px; line-height: 1.6;",
                                                              tags$li(strong("Required columns: "), "DRUG, CONCENTRATION, SCREEN, RESPONSE."),
                                                              tags$li(strong("Screen identifiers: "), "Must contain ", strong("Co-culture"), " and ", strong("Mono"),". An optional ", strong("Control"), " screen enables toxicity scoring.")
                                                      )
                                                  )
                                                ),
                                                conditionalPanel(
                                                  condition = "input.screening_type == 'Antiviral'",
                                                  div(style = "background-color: #f8f9fa; padding: 25px; border-radius: 10px; border-left: 5px solid #2B5C8F;",
                                                      h5(icon("virus-covid", class="fa-fw"), " Antiviral data format", style = "color: #2B5C8F; font-weight: 700; margin-bottom: 15px;"),
                                                      p("For evaluating viral inhibition against host cell viability.", style = "font-size: 0.95rem; color: #4A4A4A;"),
                                                      tags$ul(style = "font-size: 0.9rem; color: #4A4A4A; padding-left: 15px; line-height: 1.6;",
                                                              tags$li(strong("Required columns: "), "DRUG, CONCENTRATION, SCREEN, RESPONSE."),
                                                              tags$li(strong("Screen identifiers: "), "Must contain exactly ", strong("Co-culture"), " and ", strong("Mock"), "."),
                                                              tags$li(strong("Response values: "), strong("Host Cell Viability (%)"), " for Mock and ", strong("Viral Inhibition (%)"), " for Co-culture.")
                                                      )
                                                  )
                                                ),
                                                div(style = "margin-top: 25px; display: flex; justify-content: flex-end;",
                                                    actionLink("link_to_docs_from_welcome", tagList("See documentation ", icon("arrow-right")), style = "color: #2B5C8F; font-weight: bold; text-decoration: none; font-size: 1.05rem;")
                                                )
                                            )
                                     ),
                                     column(4,
                                            div(style = "background-color: transparent;",
                                                h4("Supported formats", style = "font-weight: 800; color: #212529; margin-bottom: 20px; font-size: 1.3rem;"),
                                                
                                                downloadLink("download_example_sens_pre", class = "tutorial-card-link",
                                                             div(class = "tutorial-card",
                                                                 h6(icon("flask-vial"), " Annotation file", style = "font-weight: 700; color: #212529; margin-bottom: 5px; font-size: 0.95rem;"),
                                                                 p("Ideal for raw plate reader outputs. Automatically computes normalized inhibition using internal controls.", style = "font-size: 0.85rem; color: #6c757d; margin: 0 0 10px 0; line-height: 1.4;"),
                                                                 div(class = "dl-overlay", icon("download"))
                                                             )
                                                ),
                                                downloadLink("download_example_sens_proc", class = "tutorial-card-link",
                                                             div(class = "tutorial-card",
                                                                 h6(icon("table"), " Processed data", style = "font-weight: 700; color: #212529; margin-bottom: 5px; font-size: 0.95rem;"),
                                                                 p("For pre-normalized data. Directly inputs viability or inhibition percentages across matched screens.", style = "font-size: 0.85rem; color: #6c757d; margin: 0 0 10px 0; line-height: 1.4;"),
                                                                 div(class = "dl-overlay", icon("download"))
                                                             )
                                                ),
                                                downloadLink("download_example_anti", class = "tutorial-card-link",
                                                             div(class = "tutorial-card",
                                                                 h6(icon("virus-covid"), " Antiviral data", style = "font-weight: 700; color: #212529; margin-bottom: 5px; font-size: 0.95rem;"),
                                                                 p("Tailored for host-pathogen setups. Evaluates viral inhibition against mock-infected host viability.", style = "font-size: 0.85rem; color: #6c757d; margin: 0 0 10px 0; line-height: 1.4;"),
                                                                 div(class = "dl-overlay", icon("download"))
                                                             )
                                                ),
                                                
                                                a(href = "https://github.com/dias-dio/CES/tree/main/R", target = "_blank", class = "tutorial-card-link", style = "margin-top: 25px; display: block;",
                                                  div(class = "tutorial-card",
                                                      h6(icon("laptop-code"), " Raw, Breeze & FIMM files", style = "font-weight: 700; color: #212529; margin-bottom: 5px; font-size: 0.95rem;"),
                                                      p("Merge raw plate reader outputs, Breeze or FIMM experimental info layouts, and annotation files into a CES-ready format using our local R script. Example files are included.", style = "font-size: 0.85rem; color: #6c757d; margin: 0 0 10px 0; line-height: 1.4;"),
                                                      div(class = "dl-overlay", icon("arrow-up-right-from-square"))
                                                  )
                                                )
                                            )
                                     )
                                   )
                               )
                             ),
                             
                             uiOutput("results_tabs_ui")
                         )
                )
              ),
              simple_footer
    ),
    # Documentation page
    nav_panel(title = "Documentation", value = "Documentation",
              fluidPage(
                div(style = "max-width: 1200px; margin: 0 auto; padding: 40px 15px;",
                    fluidRow(
                      column(3,
                             div(style = "position: sticky; top: 120px; padding: 25px; background: white; border: 1px solid #E5E4E2; border-radius: 12px; box-shadow: 0 4px 15px rgba(0,0,0,0.03);",
                                 h5("Contents", style = "font-weight: 800; color: #2B5C8F; margin-bottom: 15px;"),
                                 tags$div(style = "line-height: 2.2;",
                                          a("1. Overview", href = "#doc-overview", class="doc-sidebar-link"),
                                          a("2. Input data", href = "#doc-data-req", class="doc-sidebar-link"),
                                          a("3. Outputs", href = "#doc-outputs", class="doc-sidebar-link"),
                                          a("4. Contact", href = "#doc-contact", class="doc-sidebar-link")
                                 )
                             )
                      ),
                      column(9,
                             div(class = "glass-panel", style = "padding: 50px;",
                                 h2("Technical documentation", style = "color: #2B5C8F; font-weight: bold; margin-bottom: 35px;"),
                                 # Section 1 of the documentation
                                 div(id = "doc-overview", style="margin-bottom: 40px;",
                                     h4("1. Overview", style = "font-weight: 600;"),
                                     
                                     div(style = "display: flex; flex-direction: column; gap: 15px; margin-bottom: 25px;",
                                         div(style = "background-color: #f4f8fc; padding: 18px 22px; border-radius: 10px; border-left: 4px solid #212529;",
                                             p(strong("Quantifying compound activity in co-culture assays:"),
                                               " CES is a computational framework that distinguishes compound toxicity from enhancement or inhibition of effector-mediated cytotoxicity in multicellular co-culture systems. Two scoring models are supported: a therapeutic model that penalizes effector-cell toxicity, and a mechanistic model that adjusts for it to isolate purely effector-mediated activity.",
                                               style = "color: #4A4A4A; line-height: 1.7; margin: 0;")
                                         ),
                                         div(style = "background-color: #f4f8fc; padding: 18px 22px; border-radius: 10px; border-left: 4px solid #212529;",
                                             p(strong("Flexible modeling of complex co-culture responses:"),
                                               " The framework integrates condition-specific dose-response modeling, co-culture interaction profiling, and quantitative feature extraction to capture non-linear response landscapes and identify both enhancers and inhibitors alongside their most effective concentrations.",
                                               style = "color: #4A4A4A; line-height: 1.7; margin: 0;")
                                         )
                                     ),
                                     
                                     p(strong("The pipeline operates in four stages:"), style = "color: #4A4A4A; line-height: 1.7; font-size: 1.05rem;"),
                                     tags$ul(style = "color: #4A4A4A; line-height: 1.7;",
                                             tags$li(strong("Stage 1. Quality control and data processing."),
                                                     " For annotation file uploads, plate-level quality metrics (Z-prime, robust Z-prime, SSMD) are evaluated and viability measurements are normalized to plate-specific controls. Dose-response curves are independently fitted per condition, interpolated onto a uniform dose grid, and combined into co-culture interaction profiles according to the selected scoring model."),
                                             tags$li(strong("Stage 2. Gaussian mixture modeling."),
                                                     " Interaction profiles are modeled using a flexible mixture of up to four symmetric Gaussian components to capture non-linear response landscapes. Parameters are estimated by minimizing the sum of squared errors, with differential evolution as a fallback optimizer."),
                                             tags$li(strong("Stage 3. Feature extraction and CES computation."),
                                                     " The fitted model yields the normalized area under the curve (nAUC), maximal response (Peak), and corresponding effective dose. These features are integrated into the final Co-culture Efficacy Score."),
                                             tags$li(strong("Stage 4. Output generation."),
                                                     " The pipeline exports the CES, derived pharmacological features, effector toxicity classifications, and interactive dose-response visualizations with model diagnostics.")
                                     )
                                 ),
                                 hr(style = "margin: 25px 0; border-top: 1px solid #E5E4E2;"),
                                 # Section 2 of the documentation
                                 div(id = "doc-data-req", style="margin-bottom: 40px;",
                                     h4("2. Input data", style = "font-weight: 600;"), 
                                     p("Upload your data as ", strong(".csv"), ", ", strong(".xlsx"), ", or ", strong(".txt/.tsv"), ". Column names are case-insensitive and common synonyms are automatically mapped (e.g., ", em("DRUG_NAME"), " \u2192 ", em("DRUG"), ", ", em("CONDITION"), " \u2192 ", em("SCREEN"), "). Required columns depend on your chosen pipeline format.", style = "color: #4A4A4A;"),
                                     
                                     h5("Column reference", style = "margin-top: 25px; color: #212529; font-weight: 600;"),
                                     tags$ul(style = "color: #4A4A4A; line-height: 1.7;",
                                             tags$li(strong("PLATE"), ": Serial number (numeric) for multi-plate QC routing."),
                                             tags$li(strong("WELL"), ": Standard identifier (e.g., A1, H24) for spatial mapping."),
                                             tags$li(strong("DRUG"), ": Compound name. Internal controls are used for normalization and excluded from final tables."),
                                             tags$li(strong("CONCENTRATION"), ": Dose in nanomolar (nM). A minimum of 3 doses per drug is required."),
                                             tags$li(strong("SCREEN"), ": Condition label (Use ", strong("Co-culture"), ", ", strong("Mono"), ", ", strong("Control"), ", or ", strong("Mock"), ")."),
                                             tags$li(strong("WELL_SIGNAL / RESPONSE"), ": Raw intensity measurements or pre-computed percentages.")
                                     ),
                                     div(style = "background-color: #fff8e1; padding: 12px 20px; border-radius: 8px; border-left: 4px solid #F59E0B; margin-top: 15px;",
                                         p(icon("triangle-exclamation"), " If concentrations are not provided in nanomolar (nM), the DSS values reported in the results table may be inaccurate. All CES-derived metrics remain unaffected.",
                                           style = "color: #4A4A4A; font-size: 0.9rem; margin: 0;")
                                     ),
                                     div(style = "background-color: #f4f8fc; padding: 20px 25px; border-radius: 8px; border-left: 4px solid #2B5C8F; margin-top: 20px;",
                                         h5("Internal plate controls", style = "color: #2B5C8F; font-weight: bold; margin-bottom: 10px;"),
                                         p("Controls anchor the 0% and 100% response baselines on each plate, enabling robust normalization and removing plate-to-plate variability.", style = "color: #4A4A4A; line-height: 1.6; margin-bottom: 10px;"),
                                         tags$ul(style = "color: #4A4A4A; line-height: 1.6; margin-bottom: 0;",
                                                 tags$li(strong("Negative control (DMSO / NEG): "), "Vehicle wells with no pharmacological effect (0% inhibition). For co-culture plates, label as ", strong("DMSO2"), " or ", strong("NEG2"), "."),
                                                 tags$li(strong("Positive control (BzCl / POS): "), "A highly toxic compound that eliminates essentially all cells (100% inhibition).")
                                         )
                                     ),
                                     
                                     div(style = "margin-top: 40px; margin-bottom: 40px;",
                                         h5(icon("flask-vial"), " Drug sensitivity (Annotation file)", style = "color: #2B5C8F; font-weight: bold; margin-bottom: 15px;"),
                                         p("Select this option for raw plate-reader signals. The platform computes normalized percent inhibition using your internal plate controls.", style = "margin-bottom: 10px; color: #4A4A4A;"),
                                         tags$ul(style = "color: #4A4A4A; line-height: 1.6;",
                                                 tags$li(strong("Required columns: "), "WELL, PLATE, DRUG, CONCENTRATION, SCREEN, WELL_SIGNAL."),
                                                 tags$li(strong("Control identifiers: "), "The DRUG column must contain positive (", strong("BZCL"), "/", strong("POS"), ") and negative controls (", strong("DMSO"), "/", strong("NEG"), "). Co-culture specific negatives must use ", strong("DMSO2"), " or ", strong("NEG2"), ".")
                                         ),
                                         img(src = "tutorial_file_1.svg", style = "width: 100%; max-width: 600px; margin: 20px auto 0 auto; border-radius: 6px; border: 1px solid #E5E4E2; display: block; box-shadow: 0 2px 8px rgba(0,0,0,0.05);")
                                     ),
                                     
                                     div(style = "margin-bottom: 40px;",
                                         h5(icon("table"), " Drug sensitivity (Processed)", style = "color: #2B5C8F; font-weight: bold; margin-bottom: 15px;"),
                                         p("Select this option if your dataset is already normalized to percent viability or inhibition.", style = "margin-bottom: 10px; color: #4A4A4A;"),
                                         tags$ul(style = "color: #4A4A4A; line-height: 1.6;",
                                                 tags$li(strong("Required columns: "), "DRUG, CONCENTRATION, SCREEN, RESPONSE."),
                                                 tags$li(strong("Screen identifiers: "), "Must contain ", strong("Co-culture"), " and ", strong("Mono"), " baselines. An optional ", strong("Control"), " screen enables toxicity scoring.")
                                         ),
                                         img(src = "tutorial_file_2.svg", style = "width: 100%; max-width: 400px; margin: 15px auto 0 auto; border-radius: 6px; border: 1px solid #E5E4E2; display: block; box-shadow: 0 2px 8px rgba(0,0,0,0.05);")
                                     ),
                                     
                                     div(style = "margin-bottom: 40px;",
                                         h5(icon("virus-covid"), " Antiviral screening", style = "color: #2B5C8F; font-weight: bold; margin-bottom: 15px;"),
                                         p("Tailored for evaluating viral inhibition against host-cell viability.", style = "margin-bottom: 10px; color: #4A4A4A;"),
                                         tags$ul(style = "color: #4A4A4A; line-height: 1.6;",
                                                 tags$li(strong("Required columns: "), "DRUG, CONCENTRATION, SCREEN, RESPONSE."),
                                                 tags$li(strong("Screen identifiers: "), "Must contain exactly ", strong("Co-culture"), " and ", strong("Mock"), " (mock-infected host cell viability).")
                                         ),
                                         img(src = "tutorial_file_3.svg", style = "width: 100%; max-width: 400px; margin: 15px auto 0 auto; border-radius: 6px; border: 1px solid #E5E4E2; display: block; box-shadow: 0 2px 8px rgba(0,0,0,0.05);")
                                     ),
                                     
                                     div(style = "margin-bottom: 20px;",
                                         h5(icon("laptop-code"), " FIMM layout / Breeze experimental files", style = "color: #2B5C8F; font-weight: bold; margin-bottom: 15px;"),
                                         p("If you are processing raw data generated from FIMM or Breeze workflows, the pipeline requires three components to be merged before uploading:", style = "margin-bottom: 15px; color: #4A4A4A; line-height: 1.6;"),
                                         
                                         div(style = "display: flex; flex-direction: column; align-items: center; gap: 50px;",
                                             div(style = "text-align: center; max-width: 850px; width: 100%;",
                                                 h6(strong("1. Raw data"), style = "color: #212529; font-size: 1.1rem;"),
                                                 p("Standard plate reader outputs.", style = "font-size: 0.95rem; color: #6c757d; margin-bottom: 15px;"),
                                                 img(src = "fimm_layout_1.svg", style = "width: 100%; height: auto; border-radius: 8px; border: 1px solid #E5E4E2; box-shadow: 0 6px 18px rgba(0,0,0,0.08);")
                                             ),
                                             div(style = "text-align: center; max-width: 850px; width: 100%;",
                                                 h6(strong("2. Annotation file"), style = "color: #212529; font-size: 1.1rem;"),
                                                 p("Well mapping & concentrations.", style = "font-size: 0.95rem; color: #6c757d; margin-bottom: 15px;"),
                                                 img(src = "fimm_layout_2.svg", style = "width: 100%; height: auto; border-radius: 8px; border: 1px solid #E5E4E2; box-shadow: 0 6px 18px rgba(0,0,0,0.08);")
                                             ),
                                             div(style = "text-align: center; max-width: 850px; width: 100%;",
                                                 h6(strong("3. Experimental info"), style = "color: #212529; font-size: 1.1rem;"),
                                                 p("Breeze tracking format.", style = "font-size: 0.95rem; color: #6c757d; margin-bottom: 15px;"),
                                                 img(src = "fimm_layout_3.svg", style = "width: 100%; height: auto; border-radius: 8px; border: 1px solid #E5E4E2; box-shadow: 0 6px 18px rgba(0,0,0,0.08);")
                                             )
                                         ),
                                         div(style = "background-color: #f4f8fc; padding: 15px 20px; border-radius: 8px; border-left: 4px solid #2B5C8F; margin-top: 25px;",
                                             p(strong("Helper Script: "), "We provide a local R script to automatically parse and merge these three files into a single upload-ready format. Example raw plate files, an annotation file, and a Breeze experimental info file for a blood cancer cell line are also included for testing. ",
                                               a(href = "https://github.com/dias-dio/CES/tree/main/R", target = "_blank", style = "color: #2B5C8F; font-weight: bold; text-decoration: underline;", tagList("Download the script and examples here ", icon("arrow-up-right-from-square"))),
                                               style = "margin: 0; color: #4A4A4A; font-size: 0.95rem;")
                                         )
                                     )
                                 ),
                                 
                                 hr(style = "margin: 25px 0; border-top: 1px solid #E5E4E2;"),
                                 # Section 3 of the documentation
                                 div(id = "doc-outputs", style="margin-bottom: 40px;",
                                     h4("3. Outputs", style = "font-weight: 600;"), 
                                     p("Upon successful processing, the pipeline provides a structured results table and interactive visual diagnostics to facilitate the rapid identification of active compounds.", style = "color: #4A4A4A;"),
                                     div(style = "margin-top: 30px;",
                                         h5("Plate QC statistics (Annotation only)", style = "color: #212529; font-weight: 600; margin-bottom: 15px;"),
                                         img(src = "plate_logo.svg", style = "width: 100%; max-width: 550px; margin: 25px auto 30px auto; border-radius: 8px; border: 1px solid #E5E4E2; display: block; box-shadow: 0 4px 12px rgba(0,0,0,0.08);"),
                                         
                                         tags$ul(style = "color: #4A4A4A; line-height: 1.7;",
                                                 tags$li(strong("Plate_SSMD"), ": Strictly Standardized Mean Difference between positive and negative controls."),
                                                 tags$li(strong("Signal_Vs_BG"), ": Signal-to-background ratio."),
                                                 tags$li(strong("Z_Prime & Robust_Z_Prime"), ": Standard assay quality metrics. Values < 0.5 automatically trigger a 'Bad' flag warning.")
                                         )
                                     ),
                                     div(style = "margin-top: 40px;",
                                         h5("Results table metrics", style = "color: #212529; font-weight: 600; margin-bottom: 15px;"),
                                         img(src = "results_logo.svg", style = "width: 100%; max-width: 850px; margin: 25px auto 30px auto; border-radius: 8px; border: 1px solid #E5E4E2; display: block; box-shadow: 0 4px 12px rgba(0,0,0,0.08); image-rendering: -webkit-optimize-contrast;"),
                                         
                                         tags$ul(style = "color: #4A4A4A; line-height: 1.7;",
                                                 tags$li(strong("CES"), ": Co-culture Efficacy Score. The primary ranking metric that integrates cumulative activity and maximal efficacy into a single value. Positive scores indicate compounds that enhance effector-mediated cytotoxicity (enhancers), negative scores indicate compounds that inhibit it (inhibitors), and values near zero reflect neutral compounds with no measurable modulation."),
                                                 tags$li(strong("nAUC"), ": Normalized area under the curve of the co-culture interaction profile. Captures the cumulative drug activity across all tested concentrations, providing a measure of overall dose-dependent efficacy rather than activity at a single concentration."),
                                                 tags$li(strong("Peak"), ": The maximal response amplitude observed in the fitted interaction profile. Represents the strongest modulation achieved by the compound at any concentration, regardless of whether the overall dose-response is monotonic or bell-shaped."),
                                                 tags$li(strong("Effective_Dose"), ": The concentration at which the Peak response occurs. Compounds achieving high Peak values at lower effective doses are generally more promising candidates, as they exhibit strong modulation at pharmacologically relevant concentrations."),
                                                 tags$li(strong("IC50"), ": Half-maximal inhibitory concentration, estimated independently for each experimental condition (Co-culture, Mono, and Control where available). Provides a classical potency reference point to complement the CES-specific metrics."),
                                                 tags$li(strong("Toxic"), ": Categorical flag identifying compounds with generalized effector-cell toxicity, based on the Drug Sensitivity Score (DSS) of the Control condition exceeding a user-adjustable threshold. Only available when Control (effector-only) data is provided. Compounds flagged as toxic may show inflated co-culture efficacy due to direct effector killing rather than true immunomodulation."),
                                                 tags$li(strong("QC_fit"), ": Quality control status for the underlying dose-response model fits. A 'Fail' flag is triggered when the residual error of any condition-specific fit exceeds 30%, indicating that the model may not reliably represent the observed data for that compound."),
                                                 tags$li(strong("Excluded compounds"), ": Drugs automatically filtered by CES quality control. This occurs when the dose-response profiles across conditions exhibit biologically implausible behavior, such as concordant proliferative signals (strongly negative inhibition in both monoculture and co-culture), indicating assay artifacts rather than genuine pharmacological activity. Excluded compounds are retained in the results table for transparency but are not assigned a CES value.")
                                         ),
                                         div(style = "background-color: #f4f8fc; padding: 12px 20px; border-radius: 8px; border-left: 4px solid #73A1D4; margin-top: 20px;",
                                             p(strong("Extended download: "), "The exported file (.csv/.xlsx) expands the visual table to include individual condition Drug Sensitivity Scores (DSS), model error metrics, and dynamically scaled raw dose-response profiling measurements.", style = "color: #4A4A4A; font-size: 0.95rem; margin: 0;")
                                         )
                                     ),
                                     div(style = "margin-top: 40px;",
                                         h5("Interactive visualizations", style = "color: #212529; font-weight: 600; margin-bottom: 15px;"),
                                         img(src = "ces_plots.svg", style = "width: 100%; max-width: 600px; margin: 25px auto 30px auto; border-radius: 8px; border: 1px solid #E5E4E2; display: block; box-shadow: 0 4px 12px rgba(0,0,0,0.08);"),
                                         
                                         tags$ul(style = "color: #4A4A4A; line-height: 1.7;",
                                                 tags$li(strong("Plate QC (Annotation only): "), "Cross-screen boxplots displaying the distribution of raw well signals for each internal control type, alongside spatial scatter plots of signal intensity across plate columns. Together these reveal systematic issues such as edge effects, plate drift, or poor control separation that could compromise downstream scoring."),
                                                 tags$li(strong("CES distribution: "), "Ranked bar chart ordering all scored compounds from highest to lowest CES. Enhancers (positive CES) are colored red, inhibitors (negative CES) blue, and neutral compounds grey, providing an immediate visual summary of the overall screening landscape."),
                                                 tags$li(strong("CES vs. Effective Dose: "), "Scatter plot mapping each compound by its effective dose (x-axis, log-scaled) against its CES (y-axis). Compounds in the upper-left quadrant combine strong efficacy with low effective concentrations, highlighting the most promising candidates for further investigation."),
                                                 tags$li(strong("Dose-response modeling: "), "Per-compound interactive curve fits displaying the modeled dose-response across all experimental conditions. Raw data points are overlaid on the fitted curves, allowing visual inspection of model quality, condition-specific response differences, and the pharmacological behavior driving the integrated co-culture interaction profile.")
                                         )
                                     )
                                 ),
                                 hr(style = "margin: 25px 0; border-top: 1px solid #E5E4E2;"),
                                 div(id = "doc-contact",
                                     h4("4. Contact", style = "font-weight: 600;"), 
                                     p("For questions regarding the analytical framework or web application, please contact:", style = "margin-bottom: 15px; color: #4A4A4A;"),
                                     tags$ul(style = "list-style-type: none; padding-left: 0; color: #4A4A4A; line-height: 1.8;",
                                             tags$li(strong("CES Framework Support:"), " ", a(href="mailto:cesframework@outlook.com", "cesframework@outlook.com", style="color:#2B5C8F; font-weight: 500; text-decoration: none;"))
                                     )
                                 )
                             )
                      )
                    )
                )
              ),
              simple_footer
    ),
    # Cite page
    nav_panel(title = tagList(icon("quote-left", class = "fa-sm", style = "margin-right: 5px;"), "Cite"), value = "Cite",
              fluidPage(
                div(style = "max-width: 800px; margin: 0 auto; margin-top: 50px; padding: 20px;",
                    
                    # Citation box
                    div(style = "background: linear-gradient(135deg, #f8fafd, #eef4fa); border: 1px solid #d0e0ef; border-radius: 12px; padding: 45px; text-align: center; margin-bottom: 30px; box-shadow: 0 8px 32px 0 rgba(31, 38, 135, 0.05);",
                        h3(tagList(icon("quote-left", style = "color: #73A1D4; margin-right: 10px;"), "Citing CES"), 
                           style = "color: #2B5C8F; font-weight: 800; margin-bottom: 20px; display: flex; justify-content: center; align-items: center;"),
                        p("A manuscript detailing the CES methodology is currently in preparation.", 
                          style = "font-size: 1.1rem; color: #4A4A4A; line-height: 1.6; margin-bottom: 20px;"),
                        p(em("Citation link and DOI coming soon"), 
                          style = "color: #6c757d; font-size: 1rem; margin: 0;")
                    ),
                    
                    # Github box
                    div(style = "background: linear-gradient(135deg, #f8fafd, #eef4fa); border: 1px solid #d0e0ef; border-radius: 12px; padding: 45px; text-align: center; box-shadow: 0 8px 32px 0 rgba(31, 38, 135, 0.05);",
                        h3(tagList(icon("laptop-code", style = "color: #73A1D4; margin-right: 10px;"), "Source code and software"), 
                           style = "color: #2B5C8F; font-weight: 800; margin-bottom: 20px; display: flex; justify-content: center; align-items: center;"),
                        p("The CES computational framework and this interactive platform are entirely open-source. Explore the methodology or deploy it locally.", 
                          style = "color: #4A4A4A; font-size: 1.1rem; margin-bottom: 30px; line-height: 1.6;"),
                        
                        a(href = "https://github.com/dias-dio/CES", target = "_blank", 
                          class = "btn github-btn", 
                          style = "padding: 12px 35px; font-size: 1.05rem; font-weight: bold; text-decoration: none;",
                          tagList(icon("github"), " View repository on GitHub")
                        )
                    )
                )
              ),
              simple_footer
    ),
    nav_spacer()
  )
)