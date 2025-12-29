package com.treinamaisapi.service.avatar;

import com.treinamaisapi.common.exception.BusinessException;
import com.treinamaisapi.entity.avatar.Avatar;
import com.treinamaisapi.repository.AvatarRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class AvatarService {

    private final AvatarRepository avatarRepository;

    public Avatar buscarAvatarAtivo(String nome) {
        return avatarRepository.findByNomeAndAtivoTrue(nome)
                .orElseThrow(() ->
                        new BusinessException("Avatar inválido")
                );
    }
}
