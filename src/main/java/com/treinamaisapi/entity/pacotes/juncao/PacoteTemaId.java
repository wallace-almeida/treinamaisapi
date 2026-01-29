package com.treinamaisapi.entity.pacotes.juncao;

import jakarta.persistence.Embeddable;
import lombok.*;

import java.io.Serializable;

@Embeddable
@Getter @Setter
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode
public class PacoteTemaId implements Serializable {
    private Long pacoteId;
    private Long temaId;
}
