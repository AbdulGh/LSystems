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
		fillament = DoubleMatrix.concatHorizontally(
			var.transform(fillament),
			origin.dup() //todo check if this dup is needed (I need to learn Java)
		);
	}

	public void apply(ArrayList<Variable> vars) {
		for (Variable v : vars) apply(v);
	}

	@Override
	public void paintComponent(Graphics g) {
		super.paintComponent(g);
		Graphics2D g2d = (Graphics2D)g;

		double[][] points = fillament.transpose().toArray2();
		for (int i = 0; i < points.length - 1; ++i) {
			g2d.drawLine(20 * (int)points[i][0] + 20, 20 * (int)points[i][1] + 20, 20 * (int)points[i+1][0] + 20, 20 * (int)points[i+1][1] + 20);
		}
	}

	private static final DoubleMatrix origin = new DoubleMatrix(3,1, 0, 0, 1);

	private DoubleMatrix fillament;
}
