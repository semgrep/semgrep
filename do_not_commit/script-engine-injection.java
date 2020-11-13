package testcode.script;

import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;

public class ScriptEngineSample {

    private static ScriptEngineManager sem = new ScriptEngineManager();
    private static ScriptEngine se = sem.getEngineByExtension("js");

    // ruleid: script-engine-injection
    public static void scripting(String userInput) throws ScriptException {
        Object result = se.eval("test=1;" + userInput);
    }

    // ruleid: script-engine-injection
    public static void scripting1(String userInput) throws ScriptException {
        ScriptEngineManager scriptEngineManager = new ScriptEngineManager();
        ScriptEngine scriptEngine = scriptEngineManager.getEngineByExtension("js");
        Object result = scriptEngine.eval("test=1;" + userInput);
    }

    //ok
    public static void scriptingSafe() throws ScriptException {
        ScriptEngineManager scriptEngineManager = new ScriptEngineManager();
        ScriptEngine scriptEngine = scriptEngineManager.getEngineByExtension("js");
        String code = "var test=3;test=test*2;";
        Object result = scriptEngine.eval(code);
    }
}