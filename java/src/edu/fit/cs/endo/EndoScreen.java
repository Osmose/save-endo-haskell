package edu.fit.cs.endo;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.image.BufferedImage;

import javax.swing.JPanel;

public class EndoScreen extends JPanel {
	private BufferedImage img;
	private Color[] bgs = new Color[] {null, Color.BLACK};
	private int curColor = 0;
	
	public EndoScreen() {
		setSize(600, 600);
		setPreferredSize(new Dimension(600, 600));
	}
	
	protected void paintComponent(Graphics g) {
		super.paintComponent(g);
		
		if (bgs[curColor] != null) {
			g.setColor(bgs[curColor]);
			g.fillRect(0, 0, 600, 600);
		}
		
		g.drawImage(img, 0, 0, null);
	}
	
	public void toggleBGColor() {
		curColor = (curColor + 1) % 2;
	}

	public BufferedImage getImg() {
		return img;
	}

	public void setImg(BufferedImage img) {
		this.img = img;
		repaint();
	}
}
