package LSystems;
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
	}

	@Override
	public void paintComponent(Graphics g) {
		super.paintComponent(g);
		Graphics2D g2d = (Graphics2D)g;
	
		//todo - only do this if we've changed since the last recscaling?
		//translate to 0 (including the 1s row)
		DoubleMatrix mins = fillament.rowMins();
		fillament.addiColumnVector(mins.neg());

		//rescale to live in (w-2*margin)x(h-2*margin)
		DoubleMatrix maxes = fillament.rowMaxs(); //could probably do the following w/ jblas methods
		double xmul = (getWidth() - 2 * margin) / maxes.get(0,0);
		double ymul = (getHeight() - 2 * margin) / maxes.get(1,0);
		fillament.muliColumnVector(new DoubleMatrix(3, 1, xmul, ymul, 0));

		//add the margin (puts the 1s back in case we grow the fillament again)
		fillament.addiColumnVector(new DoubleMatrix(3, 1, margin, margin, 1));

		double[][] points = fillament.transpose().toArray2();
		for (int i = 0; i < points.length - 1; ++i) {
			g2d.drawLine((int)points[i][0], (int)points[i][1], (int)points[i+1][0], (int)points[i+1][1]);
		}
	}

	private DoubleMatrix fillament;

	private static final int margin = 10;
	private static final DoubleMatrix origin = new DoubleMatrix(3,1, 0, 0, 1);
}
