package LSystems;

//import org.jblas.DoubleMatrix;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

import javax.naming.NameNotFoundException;

import java.util.HashMap;
import java.util.ArrayList;
import java.util.regex.Pattern;


class RuleSet {
	public RuleSet() {}

	public Boolean addRule(Variable lhs, ArrayList<Variable> rhs) {
		return (rules.putIfAbsent(lhs, rhs) == null);
	}

	public int size() {
		return rules.size();
	}

	private HashMap<Variable, ArrayList<Variable>> rules = new HashMap<Variable, ArrayList<Variable>>(); //deterministic for now
}

//todo use regexes here

public class LSystem {
	public static void main(String[] args) throws Exception {
		LSystem test = new LSystem(new File("C:\\Users\\abdulg\\Desktop\\Desktop\\Java\\LSystems\\Example LSystems\\koch.L"));
	}

	public LSystem(Scanner s) throws IllegalArgumentException, NameNotFoundException {
		//look for variable declarations
		Boolean variablesLineFound = false;
		while (s.hasNextLine()) {
			String line = s.nextLine().trim();
			if (line.length() == 0) continue;
			if (line.equals("variables:")) {
				variablesLineFound = true;
				break;
			}
		}
		if (!variablesLineFound || !s.hasNextLine()) {
			throw new IllegalArgumentException("File should contain 'variables:' before some variable definitions");
		}

		//read variable definitions: type token comment
		Boolean rulesLineFound = false;
		while (s.hasNextLine()) {
			String line = s.nextLine().split("//")[0].trim();
			if (line.length() == 0) continue;
			else if (line.equals("rules:")) {
				rulesLineFound = true;
				break;
			}

			String[] split = line.split(" ");
			if (split.length < 2) {
				throw new IllegalArgumentException("File should define at least one variable like 'type token'");
			}

			String type = split[0]; //ignored for now
			String token = split[1];
			if (variables.putIfAbsent(token, new LinearVariable(token)) != null) {
				throw new IllegalArgumentException("The variable '" + token + "' was redefined");
			}
		}
		if (variables.size() == 0 || !rulesLineFound) {
			throw new IllegalArgumentException("We should have at least one variable definition followed by 'rules:'");
		}

		
		//build variable regex, h is horizontal whitespace
		ArrayList<String> quotedStrings = new ArrayList<String>();
		for (String key : variables.keySet()) quotedStrings.add(Pattern.quote(key));
		Pattern knownVars = Pattern.compile("\\h*(" + String.join(")|(", quotedStrings) + ")\\h*");

		//read rules: symbol -> list of symbols //comment
		Boolean axiomsLineFound = false;
		while (s.hasNextLine()) { //parse a rule
			//check if we just met the 'axioms:' line
			if (s.findInLine("\\h*axioms:") != null) {
				axiomsLineFound = true;
				s.nextLine();
				break;
			}
			String lhsStr = s.findInLine(knownVars);
			if (lhsStr == null) {
				String line = s.nextLine();
				if (line.length() == 0 || line.startsWith("//")) continue;
				throw new IllegalArgumentException("Unknown variable or strange rule '" + line + "'");
			}
			Variable lhs = getVariable(lhsStr);

			//parse an arrow
			if (s.findInLine("->") == null) {
				throw new IllegalArgumentException("No rule arrow found in rule");
			}

			//parse rhs variables, empty rule allowed (but pointless until we add nondeterminism)
			ArrayList<Variable> rhs = new ArrayList<Variable>();
			String rhsStr;
			while ((rhsStr = s.findInLine(knownVars)) != null) {
				rhs.add(getVariable(rhsStr));
			}

			if (!rules.addRule(lhs, rhs)) {
				throw new IllegalArgumentException("Nondeterministic rule with LHS '" + lhsStr + "'");
			}

			s.nextLine();
		}
		if (rules.size() == 0 || !axiomsLineFound) {
			throw new IllegalArgumentException("We should have at least one rule definition followed by 'axioms:'");
		}

		//finally, parse axioms
		while (s.hasNextLine()) {
			String line = s.nextLine().split("//")[0].trim();
			if (line.length() == 0) continue;
			String[] split = line.split(" ");
			String name = split[0];
			
			ArrayList<Variable> axiom = new ArrayList<Variable>();
			for (int i = 1; i < split.length; ++i) {
				axiom.add(getVariable(split[i]));
			}

			if (axioms.putIfAbsent(name, axiom) != null) {
				throw new IllegalArgumentException("Axiom '" + name + "' redefined");
			}
		}
	}
	public LSystem(String s) throws FileNotFoundException, NameNotFoundException {
		this(new Scanner(s).useDelimiter("\n"));

	}
	public LSystem(File f) throws FileNotFoundException, NameNotFoundException {
		this(new Scanner(f).useDelimiter("\n"));
	}

	private Variable getVariable(String name) throws NameNotFoundException {
		Variable var = variables.get(name.trim());
		if (var == null) {
			throw new NameNotFoundException("Undefined variable '" + name + "'");
		}
		return var;
	}

	private RuleSet rules = new RuleSet();
	private HashMap<String, Variable> variables = new HashMap<String, Variable>();
	private HashMap<String, ArrayList<Variable>> axioms = new HashMap<String, ArrayList<Variable>>();
}
