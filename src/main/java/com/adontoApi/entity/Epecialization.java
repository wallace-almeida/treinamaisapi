package com.adontoApi.entity;

import jakarta.persistence.*;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serializable;


@Data
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
@Entity
@Table(name = "SPECIALIZATION")
public class Epecialization implements Serializable {
	private static final long serialVersionUID = 1L;


	@Id
	@Basic(optional = false)
	@SequenceGenerator(name = "SQ_idSpecialization", sequenceName = "SQ_idSpecialization", allocationSize = 1, initialValue = 1)
	@GeneratedValue(strategy=GenerationType.SEQUENCE, generator = "SQ_idSpecialization")


	@Column(name="ID")
	private Long idSpecialization;

	@Column(name="NAME")
	private String name;


	public Epecialization() {
	}

	

}