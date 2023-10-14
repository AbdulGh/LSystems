import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

import javax.naming.NameNotFoundException;
import javax.swing.JFrame;

import java.util.HashMap;
import java.util.Set;
import java.util.ArrayList;
import java.util.regex.Pattern;

class RuleSet {
	public RuleSet() {}

	public boolean addRule(Variable lhs, ArrayList<Variable> rhs) {
		return (rules.putIfAbsent(lhs, rhs) == null);
	}

	public int size() {
		return rules.size();
	}

	public ArrayList<Variable> getRuleBody(Variable k) {
		return rules.get(k);
	}

	private HashMap<Variable, ArrayList<Variable>> rules = new HashMap<Variable, ArrayList<Variable>>(); //deterministic for now
}

public class LSystem {
	public static void main(String[] args) throws Exception {
		LSystem test = new LSystem(new File("Example LSystems\\koch.L"));
		ArrayList<Variable> vars = test.goFromAxioms("curve", 1);
		System.out.println(vars);
		Fractal koch = new Fractal(vars);

		JFrame frame = new JFrame();
		frame.setTitle("Koch Curve");
		frame.setResizable(true);
		frame.setSize(1000, 1000);
		frame.getContentPane().add(koch);
    	frame.setVisible(true);
	}

	public LSystem(Scanner s) throws IllegalArgumentException, NameNotFoundException {
		//look for variable declarations
		boolean variablesLineFound = false;
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

		//read variable definitions: type token  //comment
		Pattern allowedTypes = Pattern.compile("linear");
		boolean rulesLineFound = false;
		while (s.hasNextLine()) {
			if (s.findInLine("\\h*rules:") != null) { //todo compile this upfront?
				rulesLineFound = true;
				s.nextLine();
				break;
			}
			String typeStr = s.findInLine(allowedTypes);
			if (typeStr == null) {
				String line = s.nextLine();
				if (line.length() == 0 || line.startsWith("//")) continue;
				throw new IllegalArgumentException("Line '" + line + "' does not seem to start with a known type");
			}

			String symbol = s.next();
			if (symbol == null) {
				throw new IllegalArgumentException("Malformed input before '" + s.nextLine() + "'");
			}

			switch (typeStr) {
				case "linear":
					if (variables.putIfAbsent(symbol, new LinearVariable(symbol, s)) != null) {
						throw new IllegalArgumentException("The variable '" + symbol + "' was redefined");
					}
				break;
				default:
					throw new AssertionError("weird type");
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
		boolean axiomsLineFound = false;
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
		this(new Scanner(s));

	}
	public LSystem(File f) throws FileNotFoundException, NameNotFoundException {
		this(new Scanner(f));
	}
	public Set<String> getAxiomNames() {
		return axioms.keySet();
	}
	public ArrayList<Variable> goFromAxioms(String axiomName, int level) throws NameNotFoundException {
		ArrayList<Variable> current = axioms.get(axiomName);
		if (current == null) throw new NameNotFoundException("No axiom with name '" + axiomName + "'");
		return goFromAxioms(current, level);
	}

	public ArrayList<Variable> goFromAxioms(ArrayList<Variable> axiom, int level) {
		ArrayList<Variable> current = new ArrayList<Variable>(axiom);
		for (int i = 0; i < level; ++i) {
			ArrayList<Variable> next = new ArrayList<Variable>();

			for (Variable v : current) {
				ArrayList<Variable> replacement = rules.getRuleBody(v);
				if (replacement == null) next.add(v); // it's a constant if no rules defined
				else next.addAll(replacement);
			}

			current = next;
		}
		return current;
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
