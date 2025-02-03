package com.adontoApi.entity;

import java.io.Serializable;
import java.util.Date;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.Data;
import lombok.EqualsAndHashCode;

@Data
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
@Entity
@Table(name = "USERS")
public class User implements Serializable {
	private static final long serialVersionUID = 1L;

	@Id
	@Basic(optional = false)
	@SequenceGenerator(name = "SQ_ID_USER", sequenceName = "SQ_ID_USER", allocationSize = 1, initialValue = 1)
	@GeneratedValue(strategy=GenerationType.SEQUENCE, generator = "SQ_ID_USER")
	@Column(name="ID")
	@JsonProperty("idUser")  // Explicitamente renomeando para JSON
	private Long idUser;

	@Column(name="NAME")
	@JsonProperty("name")  // Explicitamente renomeando para JSON
	private String name;

	@Column(name="EMAIL")
	@JsonProperty("email")  // Explicitamente renomeando para JSON
	private String email;

	@Column(name="PASSWORD")
	@JsonProperty("password")  // Explicitamente renomeando para JSON
	private String password;

	@Column(name="PHONE")
	@JsonProperty("phone")  // Explicitamente renomeando para JSON
	private String phone;

	@Column(name="CPF")
	@JsonProperty("cpfUser")  // Explicitamente renomeando para JSON
	private String cpfUser;

	// Descomente caso queira salvar a data de inserção
    /*@Temporal(TemporalType.DATE)
    @Column(name="DT_INSERT")
    @JsonProperty("dtInsert")  // Explicitamente renomeando para JSON
    private Date dtInsert;*/

	// Descomente caso queira salvar o tipo de usuário
    @Enumerated(EnumType.STRING)
    @Column(name = "USER_TYPE")
    @JsonProperty("userType")  // Explicitamente renomeando para JSON
    private UserType userType;

	public User() {
	}
}
