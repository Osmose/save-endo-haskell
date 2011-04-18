package edu.fit.cs.endo;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.image.BufferedImage;

import javax.swing.JPanel;

@SuppressWarnings("serial")
public class EndoScreen extends JPanel {
	private BufferedImage img;
	
	public EndoScreen() {
		setSize(600, 600);
		setPreferredSize(new Dimension(600, 600));
	}
	
	protected void paintComponent(Graphics g) {
		super.paintComponent(g);
		
		g.setColor(Color.black);
		g.fillRect(0, 0, 600, 600);
		g.drawImage(img, 0, 0, null);
	}

	public BufferedImage getImg() {
		return img;
	}

	public void setImg(BufferedImage img) {
		this.img = img;
		repaint();
	}
}
