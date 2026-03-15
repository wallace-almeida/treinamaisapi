package com.treinamaisapi.service.pacoteExpirado;

import com.treinamaisapi.common.dto.desempenho.DesempenhoPorMateriaResponse;
import com.treinamaisapi.common.dto.desempenho.DesempenhoUsuarioResponse;
import com.treinamaisapi.common.dto.desempenho.EvolucaoAcertosResponse;
import com.treinamaisapi.common.exception.NotFoundException;
import com.treinamaisapi.entity.pontuacao.Pontuacao;
import com.treinamaisapi.entity.usuarios.Usuario;
import com.treinamaisapi.repository.*;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.List;

@Service
@RequiredArgsConstructor
public class PacoteExpiradoService {

    private final PacoteCompradoRepository pacoteCompradoRepository;

    @Transactional
    public void expirarPacotes() {

        int atualizados =
                pacoteCompradoRepository.expirarPacotes(LocalDateTime.now());

        if (atualizados > 0) {
            System.out.println("Pacotes expirados: " + atualizados);
        }
    }
}
