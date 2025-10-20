package com.treinamaisapi.entity.avatar;



import jakarta.persistence.*;
import lombok.*;

@Entity
@Table(name = "avatar")
@Getter @Setter @NoArgsConstructor @AllArgsConstructor @Builder
public class Avatar {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(nullable = false)
    private String nome;

    @Column(nullable = false)
    private String caminhoImagem; // URL ou caminho do arquivo local

    @Column(nullable = false)
    private Boolean ativo = true;
}
