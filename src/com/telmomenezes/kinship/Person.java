/**
 * 
 */
package com.telmomenezes.kinship;

/**
 * @author Telmo Menezes (telmo@telmomenezes.com)
 *
 */
public class Person {
	private int Id;
	private String name;
	private char sex;
	
	public Person(int Id, String name, char sex) {
		this.Id = Id;
		this.name = name;
		this.sex = sex;
	}
	
	public int getId() {
		return Id;
	}
	
	public String getName() {
		return name;
	}
	
	public char getSex() {
		return sex;
	}
}