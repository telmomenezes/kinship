/**
 * 
 */
package com.telmomenezes.kinship;

/**
 * @author Telmo Menezes (telmo@telmomenezes.com)
 *
 */
public class Person {
	private int id;
	private String name;
	private String sex;
	private int fatherId;
	private int motherId;
	
	public Person(int id, String name, String sex,
			int fatherId, int motherId) {
		this.id = id;
		this.name = name;
		this.sex = sex;
		this.fatherId = fatherId;
		this.motherId = motherId;
	}
	
	public String toString() {
		String str = "name: " + name + "; sex: " + sex
				+ "; father: " + fatherId + "; mother: " + motherId;
		return str;
	}
	
	public int getId() {
		return id;
	}
	
	public String getName() {
		return name;
	}
	
	public String getSex() {
		return sex;
	}

	public int getFatherId() {
		return fatherId;
	}

	public void setFatherId(int fatherId) {
		this.fatherId = fatherId;
	}

	public int getMotherId() {
		return motherId;
	}

	public void setMotherId(int motherId) {
		this.motherId = motherId;
	}
}