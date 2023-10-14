import javax.swing.JPanel;
import java.awt.Graphics;
import java.awt.Graphics2D;
import org.jblas.DoubleMatrix;

import java.util.ArrayList;

public class Fractal extends JPanel {

	public Fractal() {
		fillament = origin.dup();
	}

	public Fractal(ArrayList<Variable> vars) {
		this();
		apply(vars);
	}
	
	public void apply(Variable var) { //todo see if this can be done in place - might be faster to pad w/ zeros
		fillament = var.transform(fillament);
		if (var.isTranslating()) {
			fillament = DoubleMatrix.concatHorizontally(
				fillament,
				origin
			);
		}
	}

	public void apply(ArrayList<Variable> vars) {
		for (Variable v : vars) apply(v);
		System.out.println(fillament.columns);
	}

	@Override
	public void paintComponent(Graphics g) {
		super.paintComponent(g);
		Graphics2D g2d = (Graphics2D)g;

		System.out.println(fillament);

		double[][] points = fillament.transpose().toArray2();
		for (int i = 0; i < points.length - 1; ++i) {
			g2d.drawLine(10 * (int)points[i][0] + 100, 10 * (int)points[i][1] + 100, 10 * (int)points[i+1][0] + 100, 10 * (int)points[i+1][1] + 100);
		}
	}

	private static final DoubleMatrix origin = new DoubleMatrix(3,1, 0, 0, 1);
	double xmin = 0;
	double xmax = 0;
	double ymin = 0;
	double ymax = 0;
	private DoubleMatrix fillament;
}
