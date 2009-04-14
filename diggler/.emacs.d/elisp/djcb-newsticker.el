;; newsticker stuff

(require 'newsticker)
(require 'w3m)


(defun djcb-newsticker ()
  "Start newsticker"
  (interactive)
  (newsticker-show-news)
  (newsticker-buffer-force-update))

(defun djcb-newsticker-quit ()
  "Close the newsticker buffer"
  (interactive)
  (newsticker--cache-update t)
  (kill-buffer nil))

(setq
  newsticker-heading-format "%t"
  newsticker-item-format "%t"
  newsticker-hide-old-items-in-newsticker-buffer t
  newsticker-html-renderer 'w3m-region
  newsticker-retrieval-interval 0
  newsticker-use-full-width nil)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(newsticker-url-list (quote (("Clutter Blog" "http://www.clutter-project.org/blog/?feed=rss2" nil nil nil) ("Liferea" "http://beta.blogger.com/feeds/4568179161489418249/posts/full" nil nil nil) ("Stevey's Blog Rants" "http://steve-yegge.blogspot.com/feeds/posts/default" nil nil nil) ("ScottRosenberg's" "http://www.wordyard.com/feed/" nil nil nil) ("ProgLanguages" "http://lambda-the-ultimate.org/rss.xml" nil nil nil) ("EmacsWiki" "http://www.emacswiki.org/emacs/full.rss" nil nil nil) ("Airs - Ian Lance Taylor" "http://www.airs.com/blog/feed/" nil nil nil) ("The Cliffs of Inanity" "http://tromey.com/blog/?feed=rss2" nil nil nil) ("Planet GNOME" "http://planet.gnome.org/atom.xml" nil nil nil) ("emacs-fu" "http://emacs-fu.blogspot.com/feeds/posts/default" nil nil nil) ("Planet Maemo" "http://maemo.org/news/planet-maemo/rss.xml" nil nil nil) ("Ririan Project" "http://ririanproject.com/feed/" nil nil nil) ("lifehack.org" "http://www.lifehack.org/feed/" nil nil nil) ("Zen Habits" "http://feeds.feedburner.com/zenhabits" nil nil nil) ("Unclutterer" "http://unclutterer.com/feed/" nil nil nil) ("Ideas" "http://daviddfriedman.blogspot.com/feeds/posts/default" nil nil nil) ("Boing Boing" "http://www.boingboing.net/atom.xml" nil nil nil) ("gladwell.com" "http://gladwell.typepad.com/gladwellcom/atom.xml" nil nil nil) ("Word of the Day" "http://www.merriam-webster.com/word/index.xml" nil nil nil) ("FlowingData" "http://feeds.feedburner.com/FlowingData" nil nil nil) ("Micromotives" "http://www.micromotives.com/feed/" nil nil nil) ("Danger Room" "http://blog.wired.com/defense/atom.xml" nil nil nil) ("Black Belt Bayesian" "http://www.acceleratingfuture.com/steven/?feed=rss2" nil nil nil) ("Future Current" "http://www.acceleratingfuture.com/people-blog/?feed=rss2" nil nil nil) ("Life, the Universe, and Everything" "http://www.acceleratingfuture.com/tom/?feed=rss2" nil nil nil) ("Accelerating Future" "http://www.acceleratingfuture.com/michael/blog/feed/" nil nil nil) ("Wired Science" "http://blog.wired.com/wiredscience/atom.xml" nil nil nil) ("Overcoming Bias" "http://www.overcomingbias.com/atom.xml" nil nil nil) ("The Uncredible Hallq" "http://uncrediblehallq.net/blog/?feed=rss2" nil nil nil) ("Mind Hacks" "http://www.mindhacks.com/atom.xml" nil nil nil) ("The Splintered Mind" "http://schwitzsplinters.blogspot.com/feeds/posts/default" nil nil nil) ("Retrospectacle" "http://scienceblogs.com/retrospectacle/atom.xml" nil nil nil) ("The Undercover Economist" "http://blogs.ft.com/undercover/feed/" nil nil nil) ("Marginal Revolution" "http://www.marginalrevolution.com/marginalrevolution/index.rdf" nil nil nil) ("World History Blog" "http://www.worldhistoryblog.com/feeds/posts/default" nil nil nil) ("The History Blog" "http://www.thehistoryblog.com/feed" nil nil nil) ("Ann-Christin" "http://kampista.blogspot.com/atom.xml" nil nil nil) ("flux" "http://djcbflux.blogspot.com/feeds/posts/default" nil nil nil) ("Agathe" "http://battestini.net/blog/?feed=rss2" nil nil nil) ("MartinW" "http://mwolf.net/feed/" nil nil nil) ("Makoto" "http://makotonen.livejournal.com/data/rss" nil nil nil) ("Andrea" "http://andreambro.blogspot.com/atom.xml" nil nil nil) ("ChangeLog" "http://www.djcbsoftware.nl/ChangeLog/atom.xml" nil nil nil) ("Over there/here" "http://blog.winnerhed.com/feed/" nil nil nil) ("Military History Podcast" "http://geo47.libsyn.com/rss" nil nil nil) ("AAAS Science" "http://www.sciencemag.org/rss/podcast.xml" nil nil nil) ("Crypto-Gram Security Podcast" "http://crypto-gram.libsyn.com/rss" nil nil nil) ("Math Mutation" "http://www.aracnet.com/~eseligma/mm/MM_RSS_feed.xml" nil nil nil) ("Science Talk: The Podcast of Scientific American" "http://www.sciam.com/podcast/sciam_podcast_r.xml" nil nil nil) ("IT Conversations" "http://feeds.conversationsnetwork.org/channel/itc" nil nil nil) ("Nature Podcast" "http://www.nature.com/nature/podcast/rss/nature.xml" nil nil nil) ("The Philosophy Podcast" "http://feeds.feedburner.com/PhilosophyPodcast" nil nil nil) ("NPR: Science Friday Podcast" "http://www.npr.org/rss/podcast.php?id=510221" nil nil nil) ("The Math Factor Podcast" "http://mathfactor.uark.edu/feed/" nil nil nil) ("BrainScience" "http://docartemis.com/brainsciencepodcast/feed/" nil nil nil) ("All in the Mind" "http://www.abc.net.au/rn/podcast/feeds/mind.xml" nil nil nil) ("Software Engineering Radio" "http://se-radio.net/rss" nil nil nil)))))


(provide 'djcb-newsticker)