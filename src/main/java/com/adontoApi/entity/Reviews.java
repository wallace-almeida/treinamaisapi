package com.adontoApi.entity;

import jakarta.persistence.*;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serializable;
import java.util.Date;


@Data
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
@Entity
@Table(name = "REVIEWS")
public class Reviews implements Serializable {
	private static final long serialVersionUID = 1L;


	@Id
	@Basic(optional = false)
	@SequenceGenerator(name = "SQ_idReviews", sequenceName = "SQ_idReviews", allocationSize = 1, initialValue = 1)
	@GeneratedValue(strategy=GenerationType.SEQUENCE, generator = "SQ_idReviews")
	@Column(name="ID")
	private Long idReviews;

	@Column(name="PATIENT_ID")
	private long pattientId;

	@Column(name="DENTIST_ID")
	private long dentistId;

	@Column(name="RATING")
	private long rating;

	@Column(name="REVIEW")
	private String review;


	@Temporal(TemporalType.DATE)
	@Column(name="CREATED_AT")
	private Date createdAt;

	public Reviews() {
	}

	

}