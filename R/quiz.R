
rtutor.widget.quiz = function() {
  Wid = list(
    is.task = TRUE,
    parse.fun = "home.slides.quiz.block.parse",
    init.handlers = "home.slides.quiz.init.handlers",
    ui.fun = "rtutor.quiz.shiny.ui"
  )
}

home.slides.quiz.block.parse = function(...) {
  restore.point("rtutor.quiz.block.parse")
  rtutor.quiz.block.parse(..., show.points=FALSE)
}

home.slides.quiz.init.handlers = function(wid,ps=get.ps(), app=getApp(),...) {
  restore.point("home.slides.quiz.init.handlers")

  buttonHandler(wid$checkBtnId,fun = home.slides.quiz.handler, qu=wid)
}


home.slides.quiz.handler = function(qu=NULL,formValues, ps = get.ps(),opts=ps$opts,app=getApp(),...){
  restore.point("home.slides.quiz.handler")
  clicker.dir = opts$clicker.dir
  task.id = qu$task.id
  userid = first.non.null(app$userid,ps$user.name,"guest")
  lang = first.non.null(ps$opts[["lang"]], "en")


  has.run = has.clicker.task.run(task.id = task.id, clicker.dir = clicker.dir)

  # Just works like a normal quiz
  if (has.run) {
    solved = click.check.quiz(qu=qu, formValues=formValues,show.feedback=TRUE)
    return()
  }

  # Quiz has not yet been run in lecture
  # Save answer but don't show solution
  home.sub.dir = file.path(clicker.dir, "tasks", task.id,"homesub")
  if (!dir.exists(home.sub.dir))
    dir.create(home.sub.dir,recursive = TRUE)


  df = clicker.make.submit.data(values = formValues, qu=qu,task.id=task.id, tag="home",userid=userid)

  userhash = digest(userid)
  sub.file = file.path(home.sub.dir, paste0(userhash,".sub"))
  write.table(df, file=sub.file, sep=",", row.names=FALSE, col.names= FALSE)

  part=qu$parts[[length(qu$parts)]]

  if (lang=="de") {
    msg = mark_utf8("Ihre Antwort wurde gespeichert. Wenn Sie nicht an der Vorlesung teilnehmen können, zählt diese Antwort. (Antworten in der Vorlesung erhalten jedoch einen 25% Punktebonus.)")
  } else {
    msg = "Your answer has been submitted. If you cannot participate in the lecture, this submitted answer counts. (Answering in the lecture gives a 25% point bonus, however.)"
  }

  timedMessage(part$resultId,msg, millis=20000)
}

has.clicker.task.run = function(task.id, clicker.dir) {
  task.dir = file.path(clicker.dir, "tasks", task.id)
  if (!dir.exists(task.dir)) return(FALSE)

  tag.dir = file.path(task.dir, "tags")
  if (!dir.exists(tag.dir)) return(FALSE)

  tags = list.files(tag.dir)
  if (length(tags)==0) return(FALSE)
  TRUE


}
