var util = {
    // Adds a new task (function) to be called when the page is finished loading to the existing functions
    // that should be called on the page load event
    addWindowLoadEventTask : function(task) {
    if(window.attachEvent) {
        window.attachEvent('onload', task);
        } else {
            if(window.onload) {
                var curronload = window.onload;
                var newonload = function() {
                    curronload();
                    task();
                };
                window.onload = newonload;
            } else {
                window.onload = task;
            }
        }                        
    }
}